use abi_stable::{
    std_types::{
        RBoxError,
        RResult::{self},
        RString,
    },
    RMut,
};
use config::{ColorAttribute, SrgbaTuple, TermConfig};
use portable_pty::{Child, CommandBuilder, NativePtySystem, PtyPair, PtySize, PtySystem};
use std::{
    io::{Read, Write},
    sync::{
        mpsc::{channel, Sender},
        Arc,
    },
};
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{
        as_underlying_ffi_type, CustomRef, FFIArg, FFIModule, FFIValue, FfiFuture, FfiFutureExt,
        IntoFFIVal, RegisterFFIFn, VectorRef,
    },
};
use wezterm_term::{Cell, Line, Terminal};

use tokio::sync::mpsc;

#[derive(Clone)]
pub(crate) struct WriterWrapper {
    writer: Arc<parking_lot::Mutex<Box<dyn Write + Send>>>,
}

// Note: This is no bueno, but we'll need this for now
// until we figure out how to relax some of the constraints. I'm guessing
// that the FFI layer probably just needs to use Mutex instead of RwLock
// to avoid the Sync problem.
unsafe impl Send for PtyProcess {}
unsafe impl Sync for PtyProcess {}

unsafe impl Send for VirtualTerminal {}
unsafe impl Sync for VirtualTerminal {}

impl WriterWrapper {
    pub fn new(writer: Box<dyn Write + Send>) -> Self {
        Self {
            writer: Arc::new(parking_lot::Mutex::new(writer)),
        }
    }
}

impl std::io::Write for WriterWrapper {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.writer.lock().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.lock().flush()
    }
}

struct PtyProcess {
    cancellation_token_sender: Sender<()>,
    command_sender: Sender<u8>,
    async_receiver: Arc<Mutex<mpsc::UnboundedReceiver<String>>>,
    pty_system: PtyPair,
    child: Box<dyn Child + Send + Sync + 'static>,
    writer: WriterWrapper,
}

use futures::{future::Either, lock::Mutex, FutureExt};

// #[derive(Clone)]
// struct TermAction(Action);

// impl Custom for TermAction {}

#[derive(Debug)]
struct PtyProcessError(anyhow::Error);
impl std::error::Error for PtyProcessError {}

impl std::fmt::Display for PtyProcessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0)
    }
}

impl PtyProcess {
    pub fn kill(&mut self) {
        self.child.kill().ok();
        self.cancellation_token_sender.send(()).ok();
    }

    // TODO: Replace this with a proper result rather than a bool
    pub fn send_command_char(&mut self, command: char) -> bool {
        self.command_sender
            .send(command as u8)
            .map(|_| true)
            .unwrap_or(false)
    }

    // TODO: Replace this with a proper result rather than a bool
    pub fn send_command(&mut self, command: &str) -> bool {
        for byte in command.as_bytes() {
            if let Err(_) = self.command_sender.send(*byte) {
                return false;
            }
        }

        true
    }

    // TODO: rows + cols should be u16,
    // and those bounds checks should be implemented on
    // the conversion
    pub fn resize(&mut self, rows: usize, cols: usize) -> RResult<FFIValue, RBoxError> {
        match self.pty_system.master.resize(PtySize {
            rows: rows as u16,
            cols: cols as u16,
            pixel_width: 0,
            pixel_height: 0,
        }) {
            Ok(_) => RResult::ROk(FFIValue::Void),
            Err(e) => RResult::RErr(RBoxError::new(PtyProcessError(e))),
        }
    }

    // Attempt to move the bytes without cloning the heap allocation underneath?
    pub fn async_try_read_line(&mut self) -> FfiFuture<RResult<FFIValue, RBoxError>> {
        let ar = Arc::clone(&self.async_receiver);

        async move {
            let mut guard = ar.lock().await;

            let mut buffer = String::new();

            // Optimistically read as much as we can into this buffer.
            // Yield back once we have nothing else.
            while let Ok(v) = guard.try_recv() {
                buffer.push_str(&v);
            }

            let next = guard.recv();
            let timeout = futures_time::task::sleep(futures_time::time::Duration::from_millis(2));

            futures::pin_mut!(next);

            match futures::future::select(next, timeout).await {
                Either::Left((Some(message), _)) => {
                    buffer.push_str(&message);
                    RResult::ROk(FFIValue::StringV(buffer.into()))
                }
                Either::Left((None, _)) => {
                    if buffer.is_empty() {
                        RResult::ROk(FFIValue::BoolV(false))
                    } else {
                        RResult::ROk(FFIValue::StringV(buffer.into()))
                    }
                }
                Either::Right((_, fut)) => {
                    if buffer.is_empty() {
                        fut.map(|x| {
                            if let Some(message) = x {
                                buffer.push_str(&message);

                                RResult::ROk(FFIValue::StringV(buffer.into()))
                            } else {
                                RResult::ROk(FFIValue::BoolV(false))
                            }
                        })
                        .await
                    } else {
                        RResult::ROk(FFIValue::StringV(buffer.into()))
                    }
                }
            }
        }
        .into_ffi()
    }
}

impl Custom for PtyProcess {}

impl Drop for PtyProcess {
    fn drop(self: &mut PtyProcess) {
        self.kill();
    }
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/pty-process");

    module
        .register_fn("create-native-pty-system!", create_native_pty_system)
        .register_fn("kill-pty-process!", PtyProcess::kill)
        .register_fn("pty-process-send-command", PtyProcess::send_command)
        .register_fn(
            "pty-process-send-command-char",
            PtyProcess::send_command_char,
        )
        .register_fn("async-try-read-line", PtyProcess::async_try_read_line)
        .register_fn("pty-resize!", PtyProcess::resize)
        .register_fn("virtual-terminal", |pty: &mut PtyProcess| VirtualTerminal {
            terminal: Terminal::new(
                wezterm_term::TerminalSize::default(),
                Arc::new(TermConfig::new()),
                "bash",
                "0.1.0",
                Box::new(pty.writer.clone()),
            ),
            screen_iterator: ScreenCellIterator { x: 0, y: 0 },
            last_cell: None,
            scroll_up_modifier: 0,
        })
        // Raw virtual terminal!
        .register_fn("raw-virtual-terminal", || VirtualTerminal {
            terminal: Terminal::new(
                wezterm_term::TerminalSize::default(),
                Arc::new(TermConfig::new()),
                "bash",
                "0.1.0",
                Box::new(std::io::empty()),
            ),
            screen_iterator: ScreenCellIterator { x: 0, y: 0 },
            last_cell: None,
            scroll_up_modifier: 0,
        })
        .register_fn("vte/advance-bytes", VirtualTerminal::advance_bytes)
        // Advancing with immediate action
        .register_fn(
            "vte/advance-bytes-char",
            VirtualTerminal::advance_bytes_char,
        )
        .register_fn(
            "vte/advance-bytes-with-carriage-return",
            VirtualTerminal::advance_bytes_with_carriage_return,
        )
        .register_fn("vte/resize", VirtualTerminal::resize)
        .register_fn("vte/lines", VirtualTerminal::lines)
        .register_fn("vte/line->string", TermLine::as_str)
        .register_fn("vte/cursor", VirtualTerminal::cursor)
        .register_fn("vte/cursor-x", VirtualTerminal::cursor_x)
        .register_fn("vte/cursor-y", VirtualTerminal::cursor_y)
        .register_fn("vte/line->cells", |line: &mut TermLine| -> Vec<FFIValue> {
            line.line
                .cells_mut()
                .iter()
                .cloned()
                .map(|cell| TermCell { cell }.into_ffi_val().unwrap())
                .collect()
        })
        .register_fn("vte/cell->fg", |cell: &TermCell| {
            TermColorAttribute(cell.cell.attrs().foreground())
        })
        .register_fn("vte/cell->bg", |cell: &TermCell| {
            TermColorAttribute(cell.cell.attrs().background())
        })
        // Get the color attribute, map it to the one that helix uses
        // TODO: Re-use the memory - we should pass in an FFI Vector, and then just reuse it over and over.
        .register_fn(
            "term/color-attribute",
            |attribute: &TermColorAttribute| match attribute.0 {
                ColorAttribute::TrueColorWithPaletteFallback(SrgbaTuple(r, g, b, a), _) => vec![
                    (r as isize).into_ffi_val().unwrap(),
                    (g as isize).into_ffi_val().unwrap(),
                    (b as isize).into_ffi_val().unwrap(),
                    (a as isize).into_ffi_val().unwrap(),
                ]
                .into_ffi_val(),
                ColorAttribute::TrueColorWithDefaultFallback(SrgbaTuple(r, g, b, a)) => vec![
                    (r as isize).into_ffi_val().unwrap(),
                    (g as isize).into_ffi_val().unwrap(),
                    (b as isize).into_ffi_val().unwrap(),
                    (a as isize).into_ffi_val().unwrap(),
                ]
                .into_ffi_val(),
                ColorAttribute::PaletteIndex(index) => (index as usize).into_ffi_val(),
                ColorAttribute::Default => false.into_ffi_val(),
            },
        )
        .register_fn(
            "term/color-attribute-set!",
            |attribute: &TermColorAttribute, shared_vec: FFIArg| {
                if let FFIArg::VectorRef(VectorRef { mut vec, .. }) = shared_vec {
                    match attribute.0 {
                        ColorAttribute::TrueColorWithPaletteFallback(SrgbaTuple(r, g, b, a), _) => {
                            // Update in place.
                            vec[0] = FFIValue::IntV(r as isize);
                            vec[1] = FFIValue::IntV(g as isize);
                            vec[2] = FFIValue::IntV(b as isize);
                            vec[3] = FFIValue::IntV(a as isize);

                            true.into_ffi_val()
                        }
                        ColorAttribute::TrueColorWithDefaultFallback(SrgbaTuple(r, g, b, a)) => {
                            vec[0] = FFIValue::IntV(r as isize);
                            vec[1] = FFIValue::IntV(g as isize);
                            vec[2] = FFIValue::IntV(b as isize);
                            vec[3] = FFIValue::IntV(a as isize);

                            true.into_ffi_val()
                        }
                        ColorAttribute::PaletteIndex(index) => (index as usize).into_ffi_val(),
                        ColorAttribute::Default => false.into_ffi_val(),
                    }
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn("vte/cell-width", |cell: &TermCell| cell.cell.width())
        .register_fn("vte/cell-string", |cell: &TermCell| {
            cell.cell.str().to_string()
        })
        .register_fn("vte/reset-iterator!", |term: &mut VirtualTerminal| {
            term.screen_iterator.x = 0;
            term.screen_iterator.y = term.scroll_up_modifier;
            term.last_cell = None;
        })
        .register_fn("vte/advance-iterator!", |term: &mut VirtualTerminal| {
            /*
            // Attempt to grab at (x, y)
            term.last_cell = term
                .terminal
                .screen_mut()
                .get_cell(term.screen_iterator.x, term.screen_iterator.y)
                .cloned();

            // If there is nothing there, move to the next row, attempt
            // to grab something. (0, y + 1)
            if term.last_cell.is_none() {
                term.screen_iterator.y += 1;
                term.screen_iterator.x = 0;

                term.last_cell = term
                    .terminal
                    .screen_mut()
                    .get_cell(term.screen_iterator.x, term.screen_iterator.y)
                    .cloned();
            }

            term.screen_iterator.x += 1;

            let ret = term.last_cell.is_some();

            ret
            */

            let size = term.terminal.get_size();

            let (rows, cols) = (size.rows as i64 + term.scroll_up_modifier, size.cols);

            // If we still have something to snag,
            if term.screen_iterator.x < cols && term.screen_iterator.y < rows {
                term.last_cell = term
                    .terminal
                    .screen_mut()
                    .get_cell_scrollback(term.screen_iterator.x, term.screen_iterator.y as _)
                    .cloned();

                // let line_idx = term.terminal.screen_mut().phys_row(row as i64);

                // let line = term.terminal.screen_mut().lines.get_mut(line_idx)?;
                // term.last_cell = line.cells_mut().get(term.screen_iterator.x);

                term.screen_iterator.x += 1;
                return true;
            }

            if term.screen_iterator.x >= cols && term.screen_iterator.y < rows {
                term.last_cell = term
                    .terminal
                    .screen_mut()
                    .get_cell_scrollback(term.screen_iterator.x, term.screen_iterator.y as _)
                    .cloned();

                term.screen_iterator.x = 0;
                term.screen_iterator.y += 1;

                return true;
            }

            false
        })
        .register_fn("vte/iter-x", |term: &VirtualTerminal| {
            term.screen_iterator.x
        })
        .register_fn("vte/iter-y", |term: &VirtualTerminal| {
            (term.screen_iterator.y - term.scroll_up_modifier) as isize
        })
        // TODO: Add function to mutate in place
        .register_fn("vte/iter-cell-fg", |term: &VirtualTerminal| {
            if let Some(cell) = &term.last_cell {
                TermColorAttribute(cell.attrs().to_owned().foreground()).into_ffi_val()
            } else {
                false.into_ffi_val()
            }
        })
        // TODO: Add function to mutate in place
        .register_fn("vte/iter-cell-bg", |term: &VirtualTerminal| {
            if let Some(cell) = &term.last_cell {
                TermColorAttribute(cell.attrs().to_owned().background()).into_ffi_val()
            } else {
                false.into_ffi_val()
            }
        })
        .register_fn("vte/empty-cell", || {
            TermColorAttribute(ColorAttribute::default())
        })
        .register_fn(
            "vte/iter-cell-bg-fg-set-attr!",
            |term: &VirtualTerminal, bg: FFIArg, fg: FFIArg| {
                if let Some(cell) = &term.last_cell {
                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = bg {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.attrs().to_owned().background();

                            // return true.into_ffi_val();
                        } else {
                            // return false.into_ffi_val();
                        }
                    } else {
                        // return false.into_ffi_val();
                    }
                } else {
                    // false.into_ffi_val()
                }

                if let Some(cell) = &term.last_cell {
                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = fg {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.attrs().to_owned().foreground();

                            return true.into_ffi_val();
                        } else {
                            return false.into_ffi_val();
                        }
                    } else {
                        return false.into_ffi_val();
                    }
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn(
            "vte/iter-cell-bg-set-attr!",
            |term: &VirtualTerminal, val: FFIArg| {
                if let Some(cell) = &term.last_cell {
                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = val {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.attrs().to_owned().background();

                            return true.into_ffi_val();
                        } else {
                            return false.into_ffi_val();
                        }
                    } else {
                        return false.into_ffi_val();
                    }
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn(
            "vte/iter-cell-fg-set-attr!",
            |term: &VirtualTerminal, val: FFIArg| {
                if let Some(cell) = &term.last_cell {
                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = val {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.attrs().to_owned().foreground();

                            return true.into_ffi_val();
                        } else {
                            return false.into_ffi_val();
                        }
                    } else {
                        return false.into_ffi_val();
                    }
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn("vte/iter-cell-str", |term: &VirtualTerminal| {
            if let Some(cell) = &term.last_cell {
                cell.str().to_string().into_ffi_val()
            } else {
                false.into_ffi_val()
            }
        })
        .register_fn(
            "vte/iter-cell-str-set-str!",
            |term: &VirtualTerminal, mut mut_str: RMut<'_, RString>| {
                if let Some(cell) = &term.last_cell {
                    mut_str.get_mut().clear();
                    mut_str.get_mut().push_str(cell.str());

                    RResult::ROk(FFIValue::Void)
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn("vte/scroll-up", |term: &mut VirtualTerminal| {
            term.scroll_up_modifier = (term.scroll_up_modifier - 1)
                .max(0 - term.terminal.screen().scrollback_rows() as i64);
        })
        .register_fn("vte/scroll-down", |term: &mut VirtualTerminal| {
            term.scroll_up_modifier = (term.scroll_up_modifier + 1).min(0);
        });

    module
}

struct TermColorAttribute(ColorAttribute);

impl Custom for TermColorAttribute {}

fn create_native_pty_system(command: String) -> PtyProcess {
    let pty_system = NativePtySystem::default();

    let pair = pty_system
        .openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let cmd = CommandBuilder::new(command);
    let child = pair.slave.spawn_command(cmd).unwrap();

    // Read the output in another thread.
    // This is important because it is easy to encounter a situation
    // where read/write buffers fill and block either your process
    // or the spawned process.
    let (async_sender, async_receiver) = mpsc::unbounded_channel();

    let (cancellation_token_sender, cancellation_token_receiver) = channel::<()>();

    let mut reader = pair.master.try_clone_reader().unwrap();
    let mut writer = WriterWrapper::new(pair.master.take_writer().unwrap());

    let writer_clone = writer.clone();

    std::thread::spawn(move || {
        // Consume the output from the child

        let mut read_buffer = [0; 65536];

        loop {
            if let Ok(size) = reader.read(&mut read_buffer) {
                if size != 0 {
                    let r = async_sender.send(String::from_utf8_lossy(&read_buffer[..size]).into());

                    if r.is_err() {
                        break;
                    }
                }
            } else {
                break;
            }

            match cancellation_token_receiver.try_recv() {
                Ok(_) => break,
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(std::sync::mpsc::TryRecvError::Disconnected) => break,
            }
        }
    });

    // TODO: Perhaps, don't use Strings here and instead just
    // use byte strings directly. However I think Strings work
    // just find for now.
    let (command_sender, command_receiver) = channel::<u8>();

    {
        // Obtain the writer.
        // When the writer is dropped, EOF will be sent to
        // the program that was spawned.
        // It is important to take the writer even if you don't
        // send anything to its stdin so that EOF can be
        // generated, otherwise you risk deadlocking yourself.
        // let mut writer = pair.master.take_writer().unwrap();

        if cfg!(target_os = "macos") {
            // macOS quirk: the child and reader must be started and
            // allowed a brief grace period to run before we allow
            // the writer to drop. Otherwise, the data we send to
            // the kernel to trigger EOF is interleaved with the
            // data read by the reader! WTF!?
            // This appears to be a race condition for very short
            // lived processes on macOS.
            // I'd love to find a more deterministic solution to
            // this than sleeping.
            std::thread::sleep(std::time::Duration::from_millis(20));
        }

        // This example doesn't need to write anything, but if you
        // want to send data to the child, you'd set `to_write` to
        // that data and do it like this:
        // let to_write = "ls -l";
        // if !to_write.is_empty() {
        // To avoid deadlock, wrt. reading and waiting, we send
        // data to the stdin of the child in a different thread.
        std::thread::spawn(move || loop {
            while let Ok(command) = command_receiver.recv() {
                writer.write_all(&[command]).unwrap();
            }

            break;
        });
        // }
    }

    PtyProcess {
        cancellation_token_sender,
        command_sender,
        // output_receiver: rx,
        async_receiver: Arc::new(Mutex::new(async_receiver)),
        pty_system: pair,
        child,
        writer: writer_clone,
    }
}

// Have this virtual terminal receive
// inputs from the plugin, where the plugin
// then does the rendering logic.
// Get back this terminal in a way that rendering
// is reasonably easy.
struct VirtualTerminal {
    terminal: wezterm_term::Terminal,
    screen_iterator: ScreenCellIterator,
    scroll_up_modifier: i64,
    last_cell: Option<Cell>,
}

struct ScreenCellIterator {
    x: usize,
    y: i64,
}

impl Custom for VirtualTerminal {}

struct TermCell {
    cell: Cell,
}

impl Custom for TermCell {}

struct TermLine {
    line: Line,
}

impl TermLine {
    // Convert the line to a string
    fn as_str(&self) -> String {
        self.line.as_str().to_string()
    }
}

impl Custom for TermLine {}

impl VirtualTerminal {
    // Keep track of the state of the terminal
    fn advance_bytes(&mut self, bytes: &str) {
        self.terminal.advance_bytes(bytes)
    }

    fn advance_bytes_char(&mut self, bytes: char) {
        self.terminal.advance_bytes(&[bytes as u8])
    }

    fn advance_bytes_with_carriage_return(&mut self, bytes: &str) {
        let mut buf = [0; 4];

        for char in bytes.chars() {
            match char {
                '\n' => self.terminal.advance_bytes(['\n' as u8, '\r' as u8]),
                c => self.terminal.advance_bytes(c.encode_utf8(&mut buf)),
            }
        }
    }

    // Resizes the terminal
    fn resize(&mut self, rows: usize, cols: usize) {
        self.terminal.resize(wezterm_term::TerminalSize {
            rows: rows as _,
            cols: cols as _,
            pixel_width: 0,
            pixel_height: 0,
            dpi: 0,
        });
    }

    // Get the content to render
    fn lines(&mut self) -> Vec<FFIValue> {
        let screen = self.terminal.screen();

        let rows = screen.physical_rows;

        let phys_row_start = screen.phys_row(0);
        let phys_row_end = screen.phys_row(rows as i64);

        screen
            .lines_in_phys_range(phys_row_start..phys_row_end)
            .into_iter()
            .map(|line| TermLine { line }.into_ffi_val().unwrap())
            .collect()

        // let lines = screen.lines_in_phys_range()
    }

    fn cursor(&self) -> Vec<FFIValue> {
        let pos = self.terminal.cursor_pos();

        vec![
            pos.x.into_ffi_val().unwrap(),
            (pos.y as isize).into_ffi_val().unwrap(),
        ]
    }

    fn cursor_x(&self) -> usize {
        self.terminal.cursor_pos().x
    }

    fn cursor_y(&self) -> isize {
        self.terminal.cursor_pos().y as isize
    }
}
