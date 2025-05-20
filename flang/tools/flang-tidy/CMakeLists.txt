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

set(ALL_FLANG_TIDY_CHECKS
  flangTidyBugproneModule
  )

set(ALL_FLANG_TIDY_CHECKS ${ALL_FLANG_TIDY_CHECKS} PARENT_SCOPE)

add_subdirectory(tool)
