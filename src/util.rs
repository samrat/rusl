static mut VAR_COUNTER : i32 = 0;
pub fn get_unique_varname(stem: &str) -> String {
    unsafe {
        VAR_COUNTER += 1;
        stem.to_string() + &VAR_COUNTER.to_string()
    }
}    
