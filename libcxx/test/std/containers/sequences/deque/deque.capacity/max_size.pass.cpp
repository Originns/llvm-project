//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <deque>

// size_type max_size() const;

#include "asan_testing.h"
#include <cassert>
#include <deque>
#include <limits>
#include <type_traits>

#include "test_allocator.h"
#include "test_macros.h"

int main(int, char**) {
  {
    typedef limited_allocator<int, 10> A;
    typedef std::deque<int, A> C;
    C c;
    assert(c.max_size() <= 10);
    LIBCPP_ASSERT(c.max_size() == 10);
    LIBCPP_ASSERT(is_double_ended_contiguous_container_asan_correct(c));
  }
  {
    typedef limited_allocator<int, (std::size_t)-1> A;
    typedef std::deque<int, A> C;
    const C::size_type max_dist = static_cast<C::size_type>(std::numeric_limits<C::difference_type>::max());
    C c;
    assert(c.max_size() <= max_dist);
    LIBCPP_ASSERT(c.max_size() == max_dist);
    LIBCPP_ASSERT(is_double_ended_contiguous_container_asan_correct(c));
  }
  {
    typedef std::deque<char> C;
    const C::size_type max_dist = static_cast<C::size_type>(std::numeric_limits<C::difference_type>::max());
    C c;
    assert(c.max_size() <= max_dist);
    assert(c.max_size() <= alloc_max_size(c.get_allocator()));
    LIBCPP_ASSERT(is_double_ended_contiguous_container_asan_correct(c));
  }

  return 0;
}
