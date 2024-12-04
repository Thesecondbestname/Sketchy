use std::collections::HashMap;
use std::sync::Mutex;

pub struct Interner {
    // for interning the string
    map: Mutex<FxHashMap<&'static str, StrId>>,
    // for looking up a string with an i32
    vec: Mutex<Vec<&'static str>>,
    alloc: Mutex<Allocation>,
}
struct Allocation {
    // the last interned string, ensuring that always at least one string is present
    buf: String,
    // the contigous string where every string is concatonated into
    full: Vec<String>,
}

impl Allocation {
    unsafe fn alloc(&mut self, str: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + str.len() {
            let new_cap = (cap.max(str.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = std::mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }
        let interned = {
            let start = self.buf.len();
            self.buf.push_str(&str);
            &self.buf[start..]
        };
        &*(interned as *const str)
    }
}
impl Interner {
    pub fn lookup(&self, i: StrId) -> &str {
        let x = self.vec.lock().unwrap();
        x[i.0 as usize]
    }
    pub fn with_capacity(cap: usize) -> Self {
        let cap = cap.next_power_of_two();
        Self {
            map: Mutex::new(FxHashMap::default()),
            vec: Mutex::new(Vec::with_capacity(cap)),
            alloc: Mutex::new(Allocation {
                buf: String::new(),
                full: Vec::new(),
            }),
        }
    }
    pub fn intern(&self, str: &str) -> StrId {
        self.map.clear_poison();
        self.vec.clear_poison();
        self.alloc.clear_poison();
        let mut map = self.map.lock().unwrap();

        let mut vec = self.vec.lock().unwrap();
        if let Some(&id) = map.get(&str) {
            return id;
        }
        let mut alloc = self.alloc.lock().unwrap();
        let str = unsafe { alloc.alloc(str) };
        drop(alloc);
        let id = StrId(vec.len() as u32);
        map.insert(str, id);
        drop(map);
        vec.push(str);
        drop(vec);
        debug_assert!(self.lookup(id) == str);
        debug_assert!(self.intern(str) == id);

        id
    }
}
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
pub struct StrId(u32);

impl From<StrId> for &'static str {
    fn from(sym: StrId) -> Self {
        singleton().lookup(sym)
    }
}
impl From<&str> for StrId {
    fn from(s: &str) -> Self {
        singleton().intern(s)
    }
}
impl StrId {
    pub fn as_str(&self) -> &str {
        singleton().lookup(*self)
    }
}
impl std::fmt::Display for StrId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
fn singleton() -> &'static Interner {
    use std::mem::MaybeUninit;
    use std::sync::Once;
    static mut SINGLETON: MaybeUninit<Interner> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();

    // SAFETY:
    // - writing to the singleton is OK because we only do it one time
    // - the ONCE guarantees that SINGLETON is init'ed before assume_init_ref
    unsafe {
        ONCE.call_once(|| {
            SINGLETON.write(Interner::with_capacity(0));
        });
        SINGLETON.assume_init_ref()
    }
}
use std::hash::BuildHasherDefault;

/// A `HashMap` using a default Fx hasher.
///
/// Use `FxHashMap::default()`, not `new()` to create a new `FxHashMap`.
/// To create with a reserved capacity, use `FxHashMap::with_capacity_and_hasher(num, Default::default())`.
pub type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;

const ROTATE: u32 = 5;
const SEED64: u64 = 0x51_7c_c1_b7_27_22_0a_95;
#[derive(Debug, Clone)]
pub struct FxHasher {
    hash: usize,
}

impl Default for FxHasher {
    #[inline]
    fn default() -> FxHasher {
        FxHasher { hash: 0 }
    }
}

impl std::hash::Hasher for FxHasher {
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        use std::ops::BitXor;
        self.hash = {
            let mut hash = self.hash as u64;
            let mut bytes = bytes;
            while bytes.len() >= 8 {
                let word = u64::from_le_bytes(bytes[..8].try_into().unwrap());
                hash = hash.rotate_left(ROTATE).bitxor(word).wrapping_mul(SEED64);
                bytes = &bytes[8..];
            }

            if bytes.len() >= 4 {
                let word: u64 = u32::from_le_bytes(bytes[..4].try_into().unwrap()).into();
                hash = hash.rotate_left(ROTATE).bitxor(word).wrapping_mul(SEED64);
                bytes = &bytes[4..];
            }

            if bytes.len() >= 2 {
                let word: u64 = u16::from_le_bytes(bytes[..2].try_into().unwrap()).into();
                hash = hash.rotate_left(ROTATE).bitxor(word).wrapping_mul(SEED64);
                bytes = &bytes[2..];
            }

            if let Some(&byte) = bytes.first() {
                let word = u64::from(byte);
                hash = hash.rotate_left(ROTATE).bitxor(word).wrapping_mul(SEED64);
            }

            hash as usize
        };
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.hash as u64
    }
}
