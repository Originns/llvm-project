//===-- Unittests for generic implementation of sqrtf128-------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SqrtTest.h"

#include "src/__support/FPUtil/generic/sqrt.h"

LIST_SQRT_TESTS(float128, LIBC_NAMESPACE::fputil::sqrt<float128>);
