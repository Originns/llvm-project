add_flang_library(flangTidy STATIC
  FlangTidy.cpp
  FlangTidyModule.cpp
)

target_link_libraries(flangTidy
  PRIVATE
  FortranSupport
  FortranEvaluate
  FortranParser
  FortranSemantics
  LLVMSupport
  flangFrontend
  flangFrontendTool
  )

add_subdirectory(bugprone)
add_subdirectory(modernize)

set(ALL_FLANG_TIDY_CHECKS
  flangTidyBugproneModule
  flangTidyModernizeModule
  )

set(ALL_FLANG_TIDY_CHECKS ${ALL_FLANG_TIDY_CHECKS} PARENT_SCOPE)

add_subdirectory(tool)
