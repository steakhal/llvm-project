
class ConfigCategory {
  string DisplayName;
  string Description;
}

class ConfigValue {
  string FlagName;
  string ShortDescription;
  string LongDescription;
  list<string> RelatedConfigs;
  list<string> RelatedCheckers;
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

def DummyCategory : ConfigCategory {
  let DisplayName = "Dummy";
  let Description = "Dummy category description.";
}
