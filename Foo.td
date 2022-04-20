// All members are *required* unless explicitly marked 'optional'.
// All lists have at least one element.

class ConfigCategory<int display_order> {
  int DisplayOrder = display_order;
  string Name = NAME;
  string DisplayName;
  string Description;
}

def ReleasedConfig : ConfigCategory<0> {
  let DisplayName = "Released";
  let Description = [{
These options are intended for a regular end-user.
If one has unexpected checker reports, one might need to check these options
to see if any of these configs could resolve the observable behavior.

For example: <<cross-ref>>
  Setting the ``assume-controlled-environment=true`` would prevent e.g.
  ``getenv()`` ever returning ``NULL``.
  But it would also have effects on `taint-analysis` as well.
}];
}

def ExperimentalConfig : ConfigCategory<1> {
  let DisplayName = "Alpha"; // experimental
  let Description = [{
These config options are not meant for a regular end-user.
These options are considered for opt-in experimental features.
These options might have surprising effects on the analyzer engine.
One should use these with caution.
}];
}

def DeveloperConfig : ConfigCategory<2> {
  let DisplayName = "Debug"; // developer
  let Description = [{
These config options are intended for analyzer developers or tool vendors
providing frontends for the Clang Static Analyzer such as
`scan-build`_ or `CodeChecker`_.
}];
}

class ConfigValue {
  string FlagName;
  string ShortDescription;
  string LongDescription; // optional
  list<string> RelatedConfigs; // optional, should name config definitions.
  list<string> RelatedCheckers; // optional, should name checker definitions.
  ConfigCategory Category;
}

class BooleanConfigValue : ConfigValue {
  bit DefaultValue;
}

class EnumConfigValue : ConfigValue {
  string EnumName = NAME;
  list<string> Options;
  string DefaultValue;
}

class IntConfigValue : ConfigValue {
  int Min = 0;
  int Max = ?; // optional
  int DefaultValue;
}

class StringConfigValue : ConfigValue {
  string DefaultValue;
}

class UserModeDependentEnumConfigValue : ConfigValue {
  string EnumName = NAME;
  list<string> Options;
  string ShallowDefaultValue;
  string DeepDefaultValue;
}

class UserModeDependentIntConfigValue : ConfigValue {
  int ShallowMin = 0;
  int DeepMin = 0;
  int ShallowDefaultValue;
  int DeepDefaultValue;
}

/// Analyzer config options:

def UserMode : EnumConfigValue {
  let FlagName = "mode";
  let ShortDescription =
    "Controls the high-level analyzer mode, which influences the "
    "default settings for some of the lower-level config options (such as "
    "IPAMode).";
  let RelatedConfigs = ["MaxInlinableSize", "MaxNodesPerTopLevelFunction", "IPAMode"];
  let Options = ["deep", "shallow"];
  let DefaultValue = "deep";
  let Category = ReleasedConfig;
}

def ShouldIncludeImplicitDtorsInCFG : BooleanConfigValue {
  let FlagName = "cfg-implicit-dtors";
  let ShortDescription =
    "Whether or not implicit destructors for C++ objects "
    "should be included in the CFG.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def ShouldIncludeTemporaryDtorsInCFG : BooleanConfigValue {
  let FlagName = "cfg-temporary-dtors";
  let ShortDescription =
    "Whether or not the destructors for C++ temporary "
    "objects should be included in the CFG.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def ShouldIncludeLifetimeInCFG : BooleanConfigValue {
  let FlagName = "cfg-lifetime";
  let ShortDescription =
    "Whether or not end-of-lifetime information should be included in the CFG.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldIncludeLoopExitInCFG : BooleanConfigValue {
  let FlagName = "cfg-loopexit";
  let ShortDescription =
    "Whether or not the end of the loop information should "
    "be included in the CFG.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldIncludeRichConstructorsInCFG : BooleanConfigValue {
  let FlagName = "cfg-rich-constructors";
  let ShortDescription =
    "Whether or not construction site information should be "
    "included in the CFG C++ constructor elements.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def ShouldIncludeScopesInCFG : BooleanConfigValue {
  let FlagName = "cfg-scopes";
  let ShortDescription =
    "Whether or not scope information should be included in the CFG.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldIncludeDefaultInitForAggregates : BooleanConfigValue {
  let FlagName = "cfg-expand-default-aggr-inits";
  let ShortDescription =
    "Whether or not inline CXXDefaultInitializers for aggregate "
    "initialization in the CFG.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def MayInlineTemplateFunctions : BooleanConfigValue {
  let FlagName = "c++-template-inlining";
  let ShortDescription =
    "Whether or not templated functions may be considered for inlining.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def MayInlineCXXStandardLibrary : BooleanConfigValue {
  let FlagName = "c++-stdlib-inlining";
  let ShortDescription =
    "Whether or not C++ standard library functions may be "
                "considered for inlining.";
  let DefaultValue = true;
  let Category = ExperimentalConfig;
}

def MayInlineCXXAllocator : BooleanConfigValue {
  let FlagName = "c++-allocator-inlining";
  let ShortDescription =
    "Whether or not allocator call may be considered for inlining.";
  let DefaultValue = true;
  let Category = ExperimentalConfig;
}

def MayInlineCXXSharedPtrDtor : BooleanConfigValue {
  let FlagName = "c++-shared_ptr-inlining";
  let ShortDescription =
    "Whether or not the destructor of C++ 'shared_ptr' may be considered for "
    "inlining. This covers std::shared_ptr, std::tr1::shared_ptr, and "
    "boost::shared_ptr, and indeed any destructor named '~shared_ptr'.";
  let DefaultValue = false;
  let Category = ExperimentalConfig;
}

def MayInlineCXXTemporaryDtors : BooleanConfigValue {
  let FlagName = "c++-temp-dtor-inlining";
  let ShortDescription =
    "Whether C++ temporary destructors should be inlined "
    "during analysis. If temporary destructors are disabled "
    "in the CFG via the 'cfg-temporary-dtors' option, "
    "temporary destructors would not be inlined anyway.";
  let DefaultValue = true;
  let Category = ExperimentalConfig;
}

def ShouldSuppressNullReturnPaths : BooleanConfigValue {
  let FlagName = "suppress-null-return-paths";
  let ShortDescription =
    "Whether or not paths that go through null returns should be suppressed. "
    "This is a heuristic for avoiding bug reports with paths that go through "
    "inlined functions that are more defensive than their callers.";
  let DefaultValue = true;
  let Category = ExperimentalConfig;
}

def ShouldAvoidSuppressingNullArgumentPaths : BooleanConfigValue {
  let FlagName = "avoid-suppressing-null-argument-paths";
  let ShortDescription =
    "Whether a bug report should not be suppressed if its path includes a call "
    "with a null argument, even if that call has a null return. This option "
    "has no effect when ShouldSuppressNullReturnPaths is false. This is a "
    "counter-heuristic to avoid false negatives.";
  let DefaultValue = false;
  let Category = ExperimentalConfig;
}

def ShouldSuppressInlinedDefensiveChecks : BooleanConfigValue {
  let FlagName = "suppress-inlined-defensive-checks";
  let ShortDescription =
    "Whether or not diagnostics containing inlined "
    "defensive NULL checks should be suppressed.";
  let DefaultValue = true;
  let Category = ExperimentalConfig;
}

def MayInlineCXXContainerMethods : BooleanConfigValue {
  let FlagName = "c++-container-inlining";
  let ShortDescription =
    "Whether or not methods of C++ container objects may be "
    "considered for inlining.";
  let DefaultValue = false;
  let Category = ExperimentalConfig;
}

def ShouldSuppressFromCXXStandardLibrary : BooleanConfigValue {
  let FlagName = "suppress-c++-stdlib";
  let ShortDescription =
    "Whether or not diagnostics reported within the C++ "
    "standard library should be suppressed.";
  let DefaultValue = true;
  let Category = ReleasedConfig;
}

def ShouldCrosscheckWithZ3 : BooleanConfigValue {
  let FlagName = "crosscheck-with-z3";
  let ShortDescription =
    "Whether bug reports should be crosschecked with the Z3 "
    "constraint manager backend.";
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def IPAMode : UserModeDependentEnumConfigValue {
  let FlagName = "ipa";
  let ShortDescription = "Controls the mode of inter-procedural analysis.";
  let Options = ["none", "basic-inlining", "inlining", "dynamic", "dynamic-bifurcate"];
  let ShallowDefaultValue = "inlining";
  let DeepDefaultValue = "dynamic-bifurcate";
  let RelatedConfigs = ["UserMode"];
  let Category = DeveloperConfig;
}

def ShouldReportIssuesInMainSourceFile : BooleanConfigValue {
  let FlagName = "report-in-main-source-file";
  let ShortDescription =
    "Whether or not the diagnostic report should be always "
    "reported in the main source file and not the headers.";
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def ShouldWriteStableReportFilename : BooleanConfigValue {
  let FlagName = "stable-report-filename";
  let ShortDescription =
    "Deprecated: report filenames are now always stable. "
    "See also 'verbose-report-filename'.";
  let RelatedConfigs = ["ShouldWriteVerboseReportFilename"];
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def ShouldWriteVerboseReportFilename : BooleanConfigValue {
  let FlagName = "verbose-report-filename";
  let ShortDescription =
    "Whether or not the report filename should contain extra "
    "information about the issue.";
  let RelatedConfigs = ["ShouldWriteStableReportFilename"];
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def ShouldSerializeStats : BooleanConfigValue {
  let FlagName = "serialize-stats";
  let ShortDescription =
    "Whether the analyzer should serialize statistics to plist output. "
    "Statistics would be serialized in JSON format inside the main dictionary "
    "under the statistics key. Available only if compiled in assert mode or "
    "with LLVM statistics explicitly enabled.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def MayInlineObjCMethod : BooleanConfigValue {
  let FlagName = "objc-inlining";
  let ShortDescription =
    "Whether ObjectiveC inlining is enabled, false otherwise.";
  let DefaultValue = true;
  let Category = ExperimentalConfig;
}

def ShouldPrunePaths : BooleanConfigValue {
  let FlagName = "prune-paths";
  let ShortDescription =
    "Whether irrelevant parts of a bug report path should "
    "be pruned out of the final output.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def ShouldAddPopUpNotes : BooleanConfigValue {
  let FlagName = "add-pop-up-notes";
  let ShortDescription =
    "Whether pop-up notes should be added to the final output.";
  let DefaultValue = true;
  let Category = ExperimentalConfig;
}

def ShouldConditionalizeStaticInitializers : BooleanConfigValue {
  let FlagName = "cfg-conditional-static-initializers";
  let ShortDescription =
    "Whether 'static' initializers should be in conditional logic in the CFG.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def ShouldSynthesizeBodies : BooleanConfigValue {
  let FlagName = "faux-bodies";
  let ShortDescription =
    "Whether the analyzer engine should synthesize fake "
    "bodies for well-known functions.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def ShouldElideConstructors : BooleanConfigValue {
  let FlagName = "elide-constructors";
  let ShortDescription =
    "Whether elidable C++ copy-constructors and move-constructors should be "
    "actually elided during analysis. Both behaviors are allowed by the C++ "
    "standard, and the analyzer, like CodeGen, defaults to eliding. Starting "
    "with C++17 some elisions become mandatory, and in these cases the option "
    "will be ignored.";
  let DefaultValue = true;
  let Category = ReleasedConfig;
}

def ShouldInlineLambdas : BooleanConfigValue {
  let FlagName = "inline-lambdas";
  let ShortDescription =
    "Whether lambdas should be inlined. Otherwise a sink node will be "
    "generated each time a LambdaExpr is visited.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def ShouldWidenLoops : BooleanConfigValue {
  let FlagName = "widen-loops";
  let ShortDescription =
    "Whether the analysis should try to widen loops.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldUnrollLoops : BooleanConfigValue {
  let FlagName = "unroll-loops";
  let ShortDescription =
    "Whether the analysis should try to unroll loops with known bounds.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldDisplayNotesAsEvents : BooleanConfigValue {
  let FlagName = "notes-as-events";
  let ShortDescription =
    "Whether the bug reporter should transparently treat extra note diagnostic "
    "pieces as event diagnostic pieces. Useful when the diagnostic consumer "
    "doesn't support the extra note pieces.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldAggressivelySimplifyBinaryOperation : BooleanConfigValue {
  let FlagName = "aggressive-binary-operation-simplification";
  let ShortDescription =
    "Whether SValBuilder should rearrange comparisons and additive operations "
    "of symbolic expressions which consist of a sum of a symbol and a concrete "
    "integer into the format where symbols are on the left-hand side and the "
    "integer is on the right. This is only done if both symbols and both "
    "concrete integers are signed, greater than or equal to the quarter of the "
    "minimum value of the type and less than or equal to the quarter of the "
    "maximum value of that type. A + n <OP> B + m becomes A - B <OP> m - n, "
    "where A and B symbolic, n and m are integers. <OP> is any of '==', '!=', "
    "'<', '<=', '>', '>=', '+' or '-'. The rearrangement also happens with '-' "
    "instead of '+' on either or both side and also if any or both integers "
    "are missing.";
  let DefaultValue = false;
  let Category = ExperimentalConfig;
}

def ShouldEagerlyAssume : BooleanConfigValue {
  let FlagName = "eagerly-assume";
  let ShortDescription =
    "Whether we should eagerly assume evaluations of conditionals, thus, "
    "bifurcating the path. This indicates how the engine should handle "
    "expressions such as: 'x = (y != 0)'. When this is true then the "
    "subexpression 'y != 0' will be eagerly assumed to be true or false, thus "
    "evaluating it to the integers 0 or 1 respectively. The upside is that "
    "this can increase analysis precision until we have a better way to lazily "
    "evaluate such logic. The downside is that it eagerly bifurcates paths.";
  let DefaultValue = true;
  let Category = DeveloperConfig;
}

def IsNaiveCTUEnabled : BooleanConfigValue {
  let FlagName = "experimental-enable-naive-ctu-analysis";
  let ShortDescription =
    "Whether naive cross translation unit analysis is enabled. This is an "
    "experimental feature to inline functions from other translation units.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldDisplayMacroExpansions : BooleanConfigValue {
  let FlagName = "expand-macros";
  let ShortDescription =
    "Whether macros related to the bugpath should be "
    "expanded and included in the plist output.";
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def DisplayCTUProgress : BooleanConfigValue {
  let FlagName = "display-ctu-progress";
  let ShortDescription =
    "Whether to emit verbose output about "
    "the analyzer's progress related to ctu.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldTrackConditions : BooleanConfigValue {
  let FlagName = "track-conditions";
  let ShortDescription =
    "Whether to track conditions that are a control dependency of "
    "an already tracked variable.";
  let DefaultValue = true;
  let Category = ReleasedConfig;
}

def ShouldTrackConditionsDebug : BooleanConfigValue {
  let FlagName = "track-conditions-debug";
  let ShortDescription =
    "Whether to place an event at each tracked condition.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldApplyFixIts : BooleanConfigValue {
  let FlagName = "apply-fixits";
  let ShortDescription =
    "Apply the fix-it hints to the files";
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def ShouldDisplayCheckerNameForText : BooleanConfigValue {
  let FlagName = "display-checker-name";
  let ShortDescription =
    "Display the checker name for textual outputs";
  let DefaultValue = true;
  let Category = ReleasedConfig;
}

def ShouldSupportSymbolicIntegerCasts : BooleanConfigValue {
  let FlagName = "support-symbolic-integer-casts";
  let ShortDescription = "Produce cast symbols for integral types.";
  let DefaultValue = false;
  let Category = DeveloperConfig;
}

def ShouldConsiderSingleElementArraysAsFlexibleArrayMembers : BooleanConfigValue {
  let FlagName = "consider-single-element-arrays-as-flexible-array-members";
  let ShortDescription =
    "Consider single element arrays as flexible array member candidates. "
    "This will prevent the analyzer from assuming that a single element array "
    "holds a single element.";
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def ShouldAssumeControlledEnvironment : BooleanConfigValue {
  let FlagName = "assume-controlled-environment";
  let ShortDescription =
    "Whether the analyzed application runs in a controlled environment. "
    "We will assume that environment variables exist in queries and they hold "
    "no malicious data. For instance, if this option is enabled, 'getenv()' "
    "might be modeled by the analyzer to never return NULL.";
  let DefaultValue = false;
  let Category = ReleasedConfig;
}

def ShouldIgnoreBisonGeneratedFiles : BooleanConfigValue {
  let FlagName = "ignore-bison-generated-files";
  let ShortDescription =
    "If enabled, any files containing the \"/* A Bison parser, made by\" "
    "won't be analyzed.";
  let DefaultValue = true;
  let Category = ReleasedConfig;
}

def ShouldIgnoreFlexGeneratedFiles : BooleanConfigValue {
  let FlagName = "ignore-flex-generated-files";
  let ShortDescription =
    "If enabled, any files containing the \"/* A lexical scanner generated by "
    "flex\" won't be analyzed.";
  let DefaultValue = true;
  let Category = ReleasedConfig;
}

def CTUImportThreshold : IntConfigValue {
  let FlagName = "ctu-import-threshold";
  let ShortDescription =
    "The maximal amount of translation units that is considered "
    "for import when inlining functions during CTU analysis. "
    "Lowering this threshold can alleviate the memory burden of "
    "analysis with many interdependent definitions located in "
    "various translation units. This is valid only for non C++ "
    "source files.";
  let DefaultValue = 24;
  let Category = DeveloperConfig;
}

def CTUImportCppThreshold : IntConfigValue {
  let FlagName = "ctu-import-cpp-threshold";
  let ShortDescription =
    "The maximal amount of translation units that is considered "
    "for import when inlining functions during CTU analysis of C++ "
    "source files.";
  let DefaultValue = 8;
  let Category = DeveloperConfig;
}

def AlwaysInlineSize : IntConfigValue {
  let FlagName = "ipa-always-inline-size";
  let ShortDescription =
    "The size of the functions (in basic blocks), which should be considered "
    "to be small enough to always inline.";
  let DefaultValue = 3;
  let Category = DeveloperConfig;
}

def GraphTrimInterval : IntConfigValue {
  let FlagName = "graph-trim-interval";
  let ShortDescription =
    "How often nodes in the ExplodedGraph should be recycled to save memory. "
    "To disable node reclamation, set the option to 0.";
  let DefaultValue = 1000;
  let Category = DeveloperConfig;
}

def MinCFGSizeTreatFunctionsAsLarge : IntConfigValue {
  let FlagName = "min-cfg-size-treat-functions-as-large";
  let ShortDescription =
    "The number of basic blocks a function needs to have to be considered "
    "large for the 'max-times-inline-large' config option.";
  let RelatedConfigs = ["MaxTimesInlineLarge"];
  let DefaultValue = 14;
  let Category = DeveloperConfig;
}

def MaxSymbolComplexity : IntConfigValue {
  let FlagName = "max-symbol-complexity";
  let ShortDescription =
    "The maximum complexity of symbolic constraint.";
  let DefaultValue = 35;
  let Category = DeveloperConfig;
}

def MaxTimesInlineLarge : IntConfigValue {
  let FlagName = "max-times-inline-large";
  let ShortDescription =
    "The maximum times a large function could be inlined.";
  let RelatedConfigs = ["MinCFGSizeTreatFunctionsAsLarge"];
  let DefaultValue = 32;
  let Category = DeveloperConfig;
}

def MaxInlinableSize : UserModeDependentIntConfigValue {
  let FlagName = "max-inlinable-size";
  let ShortDescription =
    "The bound on the number of basic blocks in an inlined function.";
  let ShallowDefaultValue = 4;
  let DeepDefaultValue = 100;
  let Category = DeveloperConfig;
}

def MaxNodesPerTopLevelFunction : UserModeDependentIntConfigValue {
  let FlagName = "max-nodes";
  let ShortDescription =
    "The maximum number of nodes the analyzer can generate while exploring a "
    "top level function (for each exploded graph). 0 means no limit.";
  let ShallowDefaultValue = 75000;
  let DeepDefaultValue = 225000;
  let Category = DeveloperConfig;
}

def RegionStoreSmallStructLimit : IntConfigValue {
  let FlagName = "region-store-small-struct-limit";
  let ShortDescription =
    "The largest number of fields a struct can have and still be considered "
    "small This is currently used to decide whether or not it is worth forcing "
    "a LazyCompoundVal on bind. To disable all small-struct-dependent "
    "behavior, set the option to 0.";
  let DefaultValue = 2;
  let Category = DeveloperConfig;
}

def CTUDir : StringConfigValue {
  let FlagName = "ctu-dir";
  let ShortDescription = "The directory containing the CTU related files.";
  let DefaultValue = "";
  let Category = DeveloperConfig;
}

def CTUIndexName : StringConfigValue {
  let FlagName = "ctu-index-name";
  let ShortDescription =
    "The name of the file containing the CTU index of definitions. "
    "The index file maps USR-names to identifiers. An identifier "
    "can end with an '.ast' suffix, indicating the indentifier is "
    "a path to a pch-dump. Otherwise the identifier is regarded as "
    "path to a source file which is parsed on-demand. Relative "
    "paths are prefixed with ctu-dir, absolute paths are used "
    "unmodified during lookup.";
  let DefaultValue = "externalDefMap.txt";
  let Category = DeveloperConfig;
}

def CTUInvocationList : StringConfigValue {
  let FlagName = "ctu-invocation-list";
  let ShortDescription =
    "The path to the YAML format file containing a mapping from source file "
    "paths to command-line invocations represented as a list of arguments. "
    "This invocation is used produce the source-file's AST in case on-demand "
    "loading is performed. Example file-content: "
    "{/main.cpp: [clang++, /main.cpp], other.cpp: [clang++, /other.cpp]}";
  let DefaultValue = "invocations.yaml";
  let Category = DeveloperConfig;
}

def ModelPath : StringConfigValue {
  let FlagName = "model-path";
  let ShortDescription =
    "The analyzer can inline an alternative implementation written in C at the "
    "call site if the called function's body is not available. This is a path "
    "where to look for those alternative implementations (called models).";
  let DefaultValue = "";
  let Category = DeveloperConfig;
}

def CXXMemberInliningMode : EnumConfigValue {
  let FlagName = "c++-inlining";
  let ShortDescription =
    "Controls which C++ member functions will be considered for inlining.";
  let Options = ["constructors", "destructors", "methods"];
  let DefaultValue = "destructors";
  let Category = DeveloperConfig;
}

def ExplorationStrategy : EnumConfigValue {
  let FlagName = "exploration_strategy";
  let ShortDescription =
    "Determines the visitation order in the program state graph.";
  let Options = [
    "dfs", "bfs", "unexplored_first", "unexplored_first_queue",
    "unexplored_first_location_queue", "bfs_block_dfs_contents"
  ];
  let DefaultValue = "unexplored_first_queue";
  let Category = DeveloperConfig;
}

def RawSilencedCheckersAndPackages : StringConfigValue {
  let FlagName = "silence-checkers";
  let ShortDescription =
    "A semicolon separated list of checker and package names to silence. "
    "Silenced checkers will not emit reports, but the modeling remain enabled.";
  let DefaultValue = "";
  let Category = DeveloperConfig;
}
