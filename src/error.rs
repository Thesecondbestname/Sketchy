#[macro_export]
macro_rules! abort {
    // error!(target: "my_target", key1 = 42, key2 = true; "a {} event", "log")
    // error!(target: "my_target", "a {} event", "log")
    (target: $target:expr, $($arg:tt)+) => {($crate::log!(target: $target, $crate::Level::Error, $($arg)+)); std::process::exit(1)};

    // error!("a {} event", "log")
    ($($arg:tt)+) => {{
        use log;
        log::log!(log::Level::Error, $($arg)+);
        std::process::exit(1);
    }}

}
