// All members are *required* unless explicitly marked 'optional'.
// All lists have at least one element.

/// DSL description:
defvar False = 0;
defvar True  = 1;

class ConfigValue {
  string FlagName;
  string ShortDescription;
  string LongDescription; // optional
  list<string> RelatedConfigs; // optional, should name config definitions.
  list<string> RelatedCheckers; // optional, should name checker definitions.
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
        "(string) Controls the high-level analyzer mode, which influences the "
        "default settings for some of the lower-level config options (such as "
        "IPAMode).";
    let Options = ["deep", "shallow"];
    let DefaultValue = "deep";
}

def IPAMode : UserModeDependentEnumConfigValue {
  let FlagName = "ipa";
  let ShortDescription = "Controls the mode of inter-procedural analysis.";
  let Options = ["none", "basic-inlining", "inlining", "dynamic", "dynamic-bifurcate"];
  let ShallowDefaultValue = "inlining";
  let DeepDefaultValue = "dynamic-bifurcate";
}

def ShouldIncludeImplicitDtorsInCFG : BooleanConfigValue {
  let FlagName = "cfg-implicit-dtors";
  let ShortDescription =
    "Whether or not implicit destructors for C++ objects "
    "should be included in the CFG.";
  let DefaultValue = True;
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
}

def MaxInlinableSize : UserModeDependentIntConfigValue {
  let FlagName = "max-inlinable-size";
  let ShortDescription =
    "The bound on the number of basic blocks in an inlined function.";
  let ShallowDefaultValue = 4;
  let DeepDefaultValue = 100;
}

def CTUDir : StringConfigValue {
  let FlagName = "ctu-dir";
  let ShortDescription = "The directory containing the CTU related files.";
  let DefaultValue = "";
}

def CXXMemberInliningMode : EnumConfigValue {
  let FlagName = "c++-inlining";
  let ShortDescription =
    "Controls which C++ member functions will be considered for inlining.";
  let Options = ["constructors", "destructors", "methods"];
  let DefaultValue = "destructors";
}

def DisplayCTUProgress : BooleanConfigValue {
  let FlagName = "display-ctu-progress";
  let ShortDescription = "Whether to emit verbose output about the analyzer's "
                         "progress related to ctu.";
  let DefaultValue = False;
}
