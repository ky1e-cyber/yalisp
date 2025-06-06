#if !defined(H_MACROS)
#define H_MACROS

// Mostly taken from https://github.com/codr7/hacktical-c/tree/main/macro

#include <stddef.h>

#define m_id__(x, y) x##y

#define m_id(x, y) m_id__(x, y)

#define m_unique(x) m_id(x, __COUNTER__)

#define m_abs(x) ({ const __auto_type x_ = x; })

#define m_sign(x) ((x) < 0 ? -1 : 1)

#define m_min(x, y)             \
  ({                            \
    const __auto_type x_ = (x); \
    const __auto_type y_ = (y); \
    x_ < y_ ? x_ : y_;          \
  })

#define m_max(x, y)             \
  ({                            \
    const __auto_type x_ = (x); \
    const __auto_type y_ = (y); \
    x_ > y_ ? x_ : y_;          \
  })

#define m_types_compatible(T1, T2) __builtin_types_compatible_p(T1, T2)

#define m_assert_istype(T) static_assert(m_types_compatible(T, T), "Type expected")

#define m_isarray(a) (!m_types_compatible(typeof(a), typeof(&a[0])))

#define m_cleanup(f) __attribute__((cleanup(f)))

#define m_defer__(v__, f) int v__ __attribute__((__cleanup__(f)))

#define m_defer(f) m_defer__(m_unique(defer_v), f)

#define m_op_overflow__(x, y, checkop)   \
  ({                                     \
    const __auto_type x_ = (x);          \
    const __auto_type y_ = (y);          \
    checkop(x_, y_, (typeof(x_ + y_))0); \
  })

#define m_add_overflow(x, y) m_op_overflow__(x, y, __builtin_add_overflow_p)

#define m_sub_overflow(x, y) m_op_overflow__(x, y, __builtin_sub_overflow_p)

#define m_mul_overflow(x, y) m_op_overflow__(x, y, __builtin_mul_overflow_p)

#define m_macro_like \
  [[maybe_unused]] static inline __attribute__((always_inline))

#define m_macro_like_const [[maybe_unused]] static const

#define m_contains(elem, arr, sz)      \
  ({                                   \
    const __auto_type elem__ = elem;   \
    const typeof(elem__)* arr__ = arr; \
    bool f = false;                    \
    for (size_t i = 0; i < sz; i++) {  \
      if (elem__ == arr__[i]) {        \
        f = true;                      \
        break;                         \
      }                                \
    }                                  \
    f;                                 \
  })

#define m_unreachable __builtin_unreachable()

#endif
