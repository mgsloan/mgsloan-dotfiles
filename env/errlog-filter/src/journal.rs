use std::{
    ffi::{CStr, CString, c_char, c_int, c_void},
    io, ptr,
};

use crate::Result;

unsafe extern "C" {
    fn sd_journal_open(journal: *mut *mut c_void, flags: c_int) -> c_int;
    fn sd_journal_close(journal: *mut c_void);
    fn sd_journal_next(journal: *mut c_void) -> c_int;
    fn sd_journal_seek_head(journal: *mut c_void) -> c_int;
    fn sd_journal_seek_realtime_usec(journal: *mut c_void, timestamp: u64) -> c_int;
    fn sd_journal_seek_cursor(journal: *mut c_void, cursor: *const c_char) -> c_int;
    fn sd_journal_test_cursor(journal: *mut c_void, cursor: *const c_char) -> c_int;
    fn sd_journal_get_cursor(journal: *mut c_void, cursor: *mut *mut c_char) -> c_int;
    fn sd_journal_get_realtime_usec(journal: *mut c_void, timestamp: *mut u64) -> c_int;
    fn sd_journal_get_data(
        journal: *mut c_void,
        field: *const c_char,
        data: *mut *const c_void,
        length: *mut usize,
    ) -> c_int;
    fn sd_journal_set_data_threshold(journal: *mut c_void, size: usize) -> c_int;
}

fn check(result: c_int) -> io::Result<c_int> {
    if result < 0 {
        Err(io::Error::from_raw_os_error(-result))
    } else {
        Ok(result)
    }
}

// The handle stays on one thread; field data is copied before another journal call.
pub struct Journal(*mut c_void);

impl Journal {
    pub fn open() -> Result<Self> {
        let mut pointer = ptr::null_mut();
        check(unsafe { sd_journal_open(&mut pointer, 1) })?; // SD_JOURNAL_LOCAL_ONLY
        let journal = Self(pointer);
        check(unsafe { sd_journal_set_data_threshold(journal.0, 0) })?;
        Ok(journal)
    }

    pub fn next(&mut self) -> Result<bool> {
        Ok(check(unsafe { sd_journal_next(self.0) })? > 0)
    }

    pub fn head(&mut self) -> Result<bool> {
        check(unsafe { sd_journal_seek_head(self.0) })?;
        self.next()
    }

    pub fn seek_time(&mut self, timestamp: u64) -> Result<()> {
        check(unsafe { sd_journal_seek_realtime_usec(self.0, timestamp) })?;
        Ok(())
    }

    pub fn seek_cursor(&mut self, cursor: &str) -> Result<()> {
        let cursor = CString::new(cursor)?;
        check(unsafe { sd_journal_seek_cursor(self.0, cursor.as_ptr()) })?;
        Ok(())
    }

    pub fn at_cursor(&mut self, cursor: &str) -> Result<bool> {
        let cursor = CString::new(cursor)?;
        Ok(check(unsafe { sd_journal_test_cursor(self.0, cursor.as_ptr()) })? > 0)
    }

    pub fn cursor(&mut self) -> Result<String> {
        let mut pointer = ptr::null_mut();
        check(unsafe { sd_journal_get_cursor(self.0, &mut pointer) })?;
        let cursor = unsafe { CStr::from_ptr(pointer) }
            .to_string_lossy()
            .into_owned();
        unsafe { libc::free(pointer.cast()) };
        Ok(cursor)
    }

    pub fn timestamp(&mut self) -> Result<u64> {
        let mut timestamp = 0;
        check(unsafe { sd_journal_get_realtime_usec(self.0, &mut timestamp) })?;
        Ok(timestamp)
    }

    pub fn field(&mut self, field: &CStr) -> Result<Option<Vec<u8>>> {
        let mut pointer = ptr::null();
        let mut length = 0;
        let result =
            unsafe { sd_journal_get_data(self.0, field.as_ptr(), &mut pointer, &mut length) };
        if result == -libc::ENOENT {
            return Ok(None);
        }
        check(result)?;
        let data = unsafe { std::slice::from_raw_parts(pointer.cast::<u8>(), length) };
        Ok(Some(data[field.to_bytes().len() + 1..].to_vec()))
    }
}

impl Drop for Journal {
    fn drop(&mut self) {
        unsafe { sd_journal_close(self.0) };
    }
}
