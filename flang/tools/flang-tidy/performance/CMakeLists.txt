add_flang_library(flangTidyPerformanceModule STATIC
  IntegerPowerCheck.cpp
  PerformanceTidyModule.cpp
  PossibleTemporaryCheck.cpp
  PureProcedureCheck.cpp

  LINK_LIBS
  FortranSupport
  FortranParser
  FortranEvaluate
  FortranSemantics
  LLVMSupport
  flangTidy

  LINK_COMPONENTS
  Support
  FrontendOpenMP
  FrontendOpenACC
  TargetParser
  )
