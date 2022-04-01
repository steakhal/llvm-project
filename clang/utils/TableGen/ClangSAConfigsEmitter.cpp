//=- ClangSACheckersEmitter.cpp - Generate Clang SA checkers tables -*- C++ -*-
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend emits Clang Static Analyzer config tables.
//
//===----------------------------------------------------------------------===//

#include "TableGenBackends.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"

#include <algorithm>
#include <memory>
#include <utility>
#include <vector>

template <typename T> using uptr = std::unique_ptr<T>;
using namespace llvm;

static void ensureUniqueValues(const std::vector<StringRef> &List,
                               SMLoc ListLoc) {
  StringSet<> Uniques;
  bool Inserted;
  for (StringRef Item : List) {
    std::tie(std::ignore, Inserted) = Uniques.insert(Item);
    if (!Inserted)
      PrintFatalError(ListLoc,
                      "\"" + Item + "\" appears more then once in the list!\n");
  }
}

static std::vector<StringRef> parseListFieldIfDefined(Record *R,
                                                      StringRef FieldName) {
  // First check if the field is defined.
  if (const RecordVal *P = R->getValue(FieldName)) {
    if (isa<ListInit>(P->getValue())) {
      std::vector<StringRef> Result = R->getValueAsListOfStrings(FieldName);
      ensureUniqueValues(Result, P->getLoc());
      return Result;
    }
  }
  return {};
}

namespace {
struct ConfigValue;
struct ConfigCategory;
struct ParserContext {
  StringMap<uptr<ConfigValue>> Configs;
  StringMap<ConfigCategory> ConfigCategories;

  const ConfigCategory &lookupConfigCategory(Record *R) const;
};

struct ConfigCategory {
  explicit ConfigCategory(Record *R, const ParserContext &Ctx)
      : DisplayOrder(R->getValueAsInt("DisplayOrder")),
        Name(R->getValueAsString("Name")),
        DisplayName(R->getValueAsString("DisplayName")),
        Description(R->getValueAsString("Description")) {}
  const int64_t DisplayOrder;
  const StringRef Name;
  const StringRef DisplayName;
  const StringRef Description;
};

const ConfigCategory &ParserContext::lookupConfigCategory(Record *R) const {
  StringRef CategoryName = R->getValueAsString("Name");
  llvm::errs() << "looing up " << CategoryName << "\n";
  const auto It = ConfigCategories.find(CategoryName);
  assert(It != ConfigCategories.end());
  return It->getValue();
}

struct ConfigValue {
  ConfigValue(Record *R, const ParserContext &Ctx);
  virtual ~ConfigValue() = default;
  virtual raw_ostream &print(raw_ostream &OS) const = 0;

  const StringRef ConfigName;
  const StringRef FlagName;
  const StringRef ShortDescription;
  const StringRef LongDescription;
  const std::vector<StringRef> RelatedConfigs;
  const std::vector<StringRef> RelatedCheckers;
  const ConfigCategory &Category;
};

struct StringConfigValue final : ConfigValue {
  StringConfigValue(Record *R, const ParserContext &Ctx);
  ~StringConfigValue() override = default;
  raw_ostream &print(raw_ostream &OS) const override;

  const StringRef DefaultValue;
};

struct EnumConfigValue final : ConfigValue {
  EnumConfigValue(Record *R, const ParserContext &Ctx);
  ~EnumConfigValue() override = default;
  raw_ostream &print(raw_ostream &OS) const override;

  const StringRef EnumName;
  const std::vector<StringRef> Options;
  const StringRef DefaultValue;
};

struct BooleanConfigValue final : ConfigValue {
  BooleanConfigValue(Record *R, const ParserContext &Ctx);
  ~BooleanConfigValue() override = default;
  raw_ostream &print(raw_ostream &OS) const override;

  const bool DefaultValue;
};

struct IntConfigValue final : ConfigValue {
  IntConfigValue(Record *R, const ParserContext &Ctx);
  ~IntConfigValue() override = default;
  raw_ostream &print(raw_ostream &OS) const override;

  const int64_t Min;
  const Optional<int64_t> Max;
  const int64_t DefaultValue;
};

struct UserModeDependentIntConfigValue final : ConfigValue {
  UserModeDependentIntConfigValue(Record *R, const ParserContext &Ctx);
  ~UserModeDependentIntConfigValue() override = default;
  raw_ostream &print(raw_ostream &OS) const override;

  const int64_t ShallowMin;
  const int64_t DeepMin;
  const int64_t ShallowDefaultValue;
  const int64_t DeepDefaultValue;
};

struct UserModeDependentEnumConfigValue final : ConfigValue {
  StringRef EnumName;
  std::vector<StringRef> Options;
  StringRef ShallowDefaultValue;
  StringRef DeepDefaultValue;

  UserModeDependentEnumConfigValue(Record *R, const ParserContext &Ctx);
  ~UserModeDependentEnumConfigValue() override = default;
  raw_ostream &print(raw_ostream &OS) const override;
};
} // namespace

// Constructors:

ConfigValue::ConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigName(R->getName()), FlagName(R->getValueAsString("FlagName")),
      ShortDescription(R->getValueAsString("ShortDescription")),
      LongDescription(
          R->getValueAsOptionalString("LongDescription").getValueOr("")),
      RelatedConfigs(parseListFieldIfDefined(R, "RelatedConfigs")),
      RelatedCheckers(parseListFieldIfDefined(R, "RelatedCheckers")),
      Category(Ctx.lookupConfigCategory(R->getValueAsDef("Category"))) {
  assert(R->isSubClassOf("ConfigValue"));
  assert(!ConfigName.empty());

  if (FlagName.empty())
    PrintFatalError(R->getFieldLoc("FlagName"),
                    "FlagName should not be empty.");

  if (ShortDescription.empty())
    PrintFatalError(R->getFieldLoc("ShortDescription"),
                    "ShortDescription should not be empty.");

  // TODO: Enforce that !LongDescription.empty(), when we have this for all
  // configs.

  if (!RelatedConfigs.empty())
    ensureUniqueValues(RelatedConfigs, R->getFieldLoc("RelatedConfigs"));

  if (!RelatedCheckers.empty())
    ensureUniqueValues(RelatedCheckers, R->getFieldLoc("RelatedCheckers"));

  // Forbid referencing to self.
  if (is_contained(RelatedConfigs, ConfigName))
    PrintFatalError(R->getFieldLoc("RelatedConfigs"),
                    "Should not refer to itself.");
}

BooleanConfigValue::BooleanConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigValue(R, Ctx), DefaultValue(R->getValueAsBit("DefaultValue")) {}

EnumConfigValue::EnumConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigValue(R, Ctx), EnumName(R->getValueAsString("EnumName")),
      Options(R->getValueAsListOfStrings("Options")),
      DefaultValue(R->getValueAsString("DefaultValue")) {
  SMLoc ListLoc = R->getFieldLoc("Options");
  SMLoc DefaultValueLoc = R->getFieldLoc("DefaultValue");
  if (Options.size() < 2)
    PrintFatalError(ListLoc,
                    "The Options list must have at least two elements!\n");

  if (!is_contained(Options, DefaultValue)) {
    PrintError(ListLoc, "The field named `Options' must contain the value of "
                        "`DefaultValue'!\n");
    PrintFatalNote(DefaultValueLoc, "`DefaultValue' is " + DefaultValue);
  }

  ensureUniqueValues(Options, ListLoc);
}

IntConfigValue::IntConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigValue(R, Ctx), Min(R->getValueAsInt("Min")),
      Max(R->getValueAsOptionalInt("Max")),
      DefaultValue(R->getValueAsInt("DefaultValue")) {
  if (Min > DefaultValue) {
    PrintError(R->getLoc(), "The field named `Min' must be smaller or "
                            "equal to the field named `DefaultValue'!\n");
    PrintNote(R->getFieldLoc("Min"), "`Min' is " + std::to_string(Min));
    PrintFatalNote(R->getFieldLoc("DefaultValue"),
                   "`DefaultValue' is " + std::to_string(DefaultValue));
  }

  if (Max.hasValue() && DefaultValue > Max) {
    PrintError(R->getLoc(), "The field named `DefaultValue' must be smaller or "
                            "equal to the field named `DefaultValue'!\n");
    PrintNote(R->getFieldLoc("DefaultValue"),
              "`DefaultValue' is " + std::to_string(DefaultValue));
    PrintFatalNote(R->getFieldLoc("Max"), "`Max' is " + std::to_string(*Max));
  }

  if (Max.hasValue() && Min > Max) {
    PrintError(R->getLoc(), "The field named `Min' must be smaller or "
                            "equal to the field named `Max'!\n");
    PrintNote(R->getFieldLoc("Min"), "`Min' is " + std::to_string(Min));
    PrintFatalNote(R->getFieldLoc("Max"), "`Max' is " + std::to_string(*Max));
  }
}

StringConfigValue::StringConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigValue(R, Ctx), DefaultValue(R->getValueAsString("DefaultValue")) {}

UserModeDependentEnumConfigValue::UserModeDependentEnumConfigValue(
    Record *R, const ParserContext &Ctx)
    : ConfigValue(R, Ctx), EnumName(R->getValueAsString("EnumName")),
      Options(R->getValueAsListOfStrings("Options")),
      ShallowDefaultValue(R->getValueAsString("ShallowDefaultValue")),
      DeepDefaultValue(R->getValueAsString("DeepDefaultValue")) {
  SMLoc ListLoc = R->getFieldLoc("Options");
  SMLoc ShallowDefaultValueLoc = R->getFieldLoc("ShallowDefaultValue");
  SMLoc DeepDefaultValueLoc = R->getFieldLoc("DeepDefaultValue");
  if (Options.size() < 2)
    PrintFatalError(ListLoc,
                    "The Options list must have at least two elements!\n");

  if (!is_contained(Options, ShallowDefaultValue)) {
    PrintError(ListLoc, "The field named `Options' must contain the value of "
                        "`ShallowDefaultValue'!\n");
    PrintFatalNote(ShallowDefaultValueLoc,
                   "`ShallowDefaultValue' is " + ShallowDefaultValue);
  }

  if (!is_contained(Options, DeepDefaultValue)) {
    PrintError(ListLoc, "The field named `Options' must contain the value of "
                        "`DeepDefaultValue'!\n");
    PrintFatalNote(DeepDefaultValueLoc,
                   "`DeepDefaultValue' is " + DeepDefaultValue);
  }

  ensureUniqueValues(Options, ListLoc);
}

UserModeDependentIntConfigValue::UserModeDependentIntConfigValue(
    Record *R, const ParserContext &Ctx)
    : ConfigValue(R, Ctx), ShallowMin(R->getValueAsInt("ShallowMin")),
      DeepMin(R->getValueAsInt("DeepMin")),
      ShallowDefaultValue(R->getValueAsInt("ShallowDefaultValue")),
      DeepDefaultValue(R->getValueAsInt("DeepDefaultValue")) {

  if (ShallowMin > ShallowDefaultValue) {
    PrintError(R->getLoc(),
               "The field named `ShallowMin' must be smaller or "
               "equal to the field named `ShallowDefaultValue'!\n");
    PrintNote(R->getFieldLoc("ShallowMin"),
              "`ShallowMin' is " + std::to_string(ShallowMin));
    PrintFatalNote(R->getFieldLoc("ShallowDefaultValue"),
                   "`ShallowDefaultValue' is " +
                       std::to_string(ShallowDefaultValue));
  }

  if (DeepMin > DeepDefaultValue) {
    PrintError(R->getLoc(), "The field named `DeepMin' must be smaller or "
                            "equal to the field named `DeepDefaultValue'!\n");
    PrintNote(R->getFieldLoc("DeepMin"),
              "`DeepMin' is " + std::to_string(DeepMin));
    PrintFatalNote(R->getFieldLoc("DeepDefaultValue"),
                   "`DeepDefaultValue' is " + std::to_string(DeepDefaultValue));
  }
}

// Print functions:

static raw_ostream &quoted(raw_ostream &OS, StringRef Str) {
  OS << "\"";
  OS.write_escaped(Str);
  return OS << "\"";
}

raw_ostream &BooleanConfigValue::print(raw_ostream &OS) const {
  OS << "ANALYZER_OPTION(bool, " << ConfigName << ", ";
  quoted(OS, FlagName) << ", ";
  quoted(OS, ShortDescription) << ", ";
  OS << (DefaultValue ? "true" : "false") << ")";
  return OS;
}

raw_ostream &EnumConfigValue::print(raw_ostream &OS) const {
  OS << "ANALYZER_OPTION(StringRef, " << ConfigName << ", ";
  quoted(OS, FlagName) << ", \"";
  OS.write_escaped(ShortDescription) << " Value: \\\"";
  interleave(
      Options, [&](StringRef Item) { OS.write_escaped(Item); },
      [&] { OS << "\\\", \\\""; });
  OS << "\\\".\""
     << ", ";
  quoted(OS, DefaultValue) << ")";
  return OS;
}

raw_ostream &IntConfigValue::print(raw_ostream &OS) const {
  OS << "ANALYZER_OPTION(unsigned, " << ConfigName << ", ";
  quoted(OS, FlagName) << ", ";
  quoted(OS, ShortDescription) << ", " << DefaultValue << "u)";
  return OS;
}

raw_ostream &StringConfigValue::print(raw_ostream &OS) const {
  OS << "ANALYZER_OPTION(StringRef, " << ConfigName << ", ";
  quoted(OS, FlagName) << ", ";
  quoted(OS, ShortDescription) << ", ";
  quoted(OS, DefaultValue) << ")";
  return OS;
}

raw_ostream &UserModeDependentEnumConfigValue::print(raw_ostream &OS) const {
  OS << "ANALYZER_OPTION_DEPENDS_ON_USER_MODE(StringRef, " << ConfigName
     << ", \"";
  quoted(OS, FlagName) << ", \"";
  OS.write_escaped(ShortDescription) << " Value: \\\"";
  interleave(
      Options, [&](StringRef Item) { OS.write_escaped(Item); },
      [&] { OS << "\\\", \\\""; });
  OS << "\\\".\""
     << ", ";
  OS << "/* SHALLOW_VAL */ ";
  quoted(OS, ShallowDefaultValue) << ", ";
  OS << "/* DEEP_VAL */ ";
  quoted(OS, DeepDefaultValue) << ")";
  return OS;
}

raw_ostream &UserModeDependentIntConfigValue::print(raw_ostream &OS) const {
  OS << "ANALYZER_OPTION_DEPENDS_ON_USER_MODE(unsigned, " << ConfigName
     << ", \"";
  quoted(OS, FlagName) << ", ";
  quoted(OS, ShortDescription) << ", ";
  OS << "/* SHALLOW_VAL */ " << ShallowDefaultValue << ", ";
  OS << "/* DEEP_VAL */ " << DeepDefaultValue << ")";
  return OS;
}

// Factory function:
static std::unique_ptr<ConfigValue>
parseSingleConfigValue(Record *R, const ParserContext &Ctx,
                       StringRef DirectBaseName) {
  if ("BooleanConfigValue" == DirectBaseName)
    return std::make_unique<BooleanConfigValue>(R, Ctx);
  if ("EnumConfigValue" == DirectBaseName)
    return std::make_unique<EnumConfigValue>(R, Ctx);
  if ("IntConfigValue" == DirectBaseName)
    return std::make_unique<IntConfigValue>(R, Ctx);
  if ("StringConfigValue" == DirectBaseName)
    return std::make_unique<StringConfigValue>(R, Ctx);
  if ("UserModeDependentEnumConfigValue" == DirectBaseName)
    return std::make_unique<UserModeDependentEnumConfigValue>(R, Ctx);
  if ("UserModeDependentIntConfigValue" == DirectBaseName)
    return std::make_unique<UserModeDependentIntConfigValue>(R, Ctx);

  PrintFatalError(R->getLoc(),
                  "Record `" + DirectBaseName +
                      "' is unhandled by the \"gen-clang-sa-configs\" "
                      "tablegen backend!\n");
}

static void parseConfigValues(RecordKeeper &Records, ParserContext &Ctx) {
  assert(!Ctx.ConfigCategories.empty() &&
         "Parse config categories before parsing config values.");

  for (Record *R : Records.getAllDerivedDefinitions("ConfigValue")) {
    SmallVector<Record *, 1> DirectBases;
    R->getDirectSuperClasses(DirectBases);
    assert(!DirectBases.empty());

    if (DirectBases.size() > 1)
      PrintFatalError(R->getLoc(),
                      "Record `" + R->getName() +
                          "' should inherit from only a single Record!\n");

    StringRef DirectBaseName = DirectBases[0]->getName();
    Ctx.Configs.insert(std::make_pair(
        DirectBaseName, parseSingleConfigValue(R, Ctx, DirectBaseName)));
  }
}

static void parseConfigCategories(RecordKeeper &Records, ParserContext &Ctx) {
  for (Record *R : Records.getAllDerivedDefinitions("ConfigCategory")) {
    llvm::errs() << "Parsed cat: " << R->getValueAsString("DisplayName")
                 << "\n";
    Ctx.ConfigCategories.insert(
        std::make_pair(R->getValueAsString("Name"), ConfigCategory{R, Ctx}));
  }
}

static void ensureAllConfigValueKindsAreUsed(const RecordKeeper &Records,
                                             const StringSet<> &SeenClasses) {
  for (const auto &MapPair : Records.getClasses()) {
    const std::string &Name = MapPair.first;
    Record *R = MapPair.second.get();
    if (R->isSubClassOf("ConfigValue") && !SeenClasses.contains(Name)) {
      PrintWarning(R->getLoc(),
                   "Record `" + Name + "' is unused; consider removing it.\n");
    }
  }
}

static void ensureAllCrossreferencesAreValid(RecordKeeper &Records,
                                             const ParserContext &Ctx) {
  for (const auto &Entry : Ctx.Configs) {
    for (StringRef RelatedConfigName : Entry.getValue()->RelatedConfigs) {
      if (Ctx.Configs.count(RelatedConfigName) == 0) {
        SMLoc ListLoc =
            Records.getDef(Entry.getKey())->getFieldLoc("RelatedConfigs");

        PrintFatalError(ListLoc,
                        "The field named `RelatedConfigs' refers to \"" +
                            RelatedConfigName +
                            "\", but no record was defined deriving from "
                            "`ConfigValue' with that name!\n");
      }
    }
  }
}

// This reduces the chance of copy-paste mistakes.
static void ensureGlobalUniqueness(RecordKeeper &Records,
                                   const ParserContext &Ctx) {
  auto CheckField = [&Records](StringMap<const ConfigValue *> &Aggregator,
                               StringRef FieldName,
                               const ConfigValue *Current) {
    StringMap<const ConfigValue *>::iterator PrevPlace;
    bool Inserted;
    std::tie(PrevPlace, Inserted) =
        Aggregator.insert(std::make_pair(Current->FlagName, Current));
    if (Inserted)
      return;

    SMLoc CurrFlagNameLoc =
        Records.getDef(Current->ConfigName)->getFieldLoc(FieldName);
    SMLoc PrevFlagNameLoc =
        Records.getDef(PrevPlace->second->ConfigName)->getFieldLoc(FieldName);
    PrintError(CurrFlagNameLoc, "The \"" + Current->FlagName +
                                    "\" value has been already used!\n");
    PrintFatalNote(PrevFlagNameLoc, "Previously used by the `" +
                                        Current->ConfigName + "' record.\n");
  };

  StringMap<const ConfigValue *> FlagNames;
  StringMap<const ConfigValue *> ShortDescriptions;
  StringMap<const ConfigValue *> LongDescriptions;

  for (const auto &Entry : Ctx.Configs) {
    const ConfigValue *C = Entry.getValue().get();
    CheckField(FlagNames, "FlagName", C);
    CheckField(ShortDescriptions, "ShortDescription", C);
    CheckField(LongDescriptions, "LongDescription", C);
  }
}

static void checkDescriptionLengths(RecordKeeper &Records,
                                    const ConfigValue *C) {
  // TODO: Remove this check if the `LongDescription` field is no longer
  // optional.
  if (C->LongDescription.empty())
    return;

  if (C->ShortDescription.size() < C->LongDescription.size())
    return;

  SMLoc LongDescLoc =
      Records.getDef(C->ConfigName)->getFieldLoc("LongDescription");
  PrintFatalError(LongDescLoc,
                  "The `LongDescription' field is shorter than the "
                  "`ShortDescription' field!\n");
}

static void performSemanticChecks(RecordKeeper &Records,
                                  const ParserContext &Ctx) {
  StringSet<> SeenClasses;
  for (const auto &Entry : Ctx.Configs) {
    SeenClasses.insert(
        Records.getDef(Entry.getValue()->ConfigName)->getType()->getAsString());
  }

  // Check if we have unhandled 'ConfigValue' classes in the tablegen file.
  ensureAllConfigValueKindsAreUsed(Records, SeenClasses);

  // Check if any of the 'ConfigValues' refer to a non-existing 'ConfigValue'
  // in the 'RelatedConfigs' list.
  ensureAllCrossreferencesAreValid(Records, Ctx);

  // Check if any of the 'ConfigValues' use the same 'FlagName',
  // 'ShortDescription' or 'LongDescription' values.
  ensureGlobalUniqueness(Records, Ctx);

  for (const auto &Entry : Ctx.Configs)
    checkDescriptionLengths(Records, Entry.getValue().get());
}

static void printAnalyzerOptions(const ParserContext &Ctx, raw_ostream &OS) {
  OS <<
      R"header(// This file is automatically generated. Do not edit this file by hand.

#ifndef LLVM_ADT_STRINGREF_H
#error This .def file is expected to be included in translation units where \
"llvm/ADT/StringRef.h" is already included!
#endif

#ifdef ANALYZER_OPTION
#ifndef ANALYZER_OPTION_DEPENDS_ON_USER_MODE
#error If you didnt include this file with the intent of generating methods, \
define both 'ANALYZER_OPTION' and 'ANALYZER_OPTION_DEPENDS_ON_USER_MODE' macros!
#endif
#endif

#ifndef ANALYZER_OPTION_DEPENDS_ON_USER_MODE
#ifdef ANALYZER_OPTION
#error If you didnt include this file with the intent of generating methods, \
define both 'ANALYZER_OPTION' and 'ANALYZER_OPTION_DEPENDS_ON_USER_MODE' macros!
#endif
#endif

#ifndef ANALYZER_OPTION
/// Create a new analyzer option, but dont generate a method for it in
/// AnalyzerOptions.
///
///   TYPE - The type of the option object that will be stored in
///          AnalyzerOptions. This file is expected to be icluded in translation
///          units where AnalyzerOptions.h is included, so types from that
///          header should be used.
///   NAME - The name of the option object.
///   CMDFLAG - The command line flag for the option.
///             (-analyzer-config CMDFLAG=VALUE)
///   DESC - Description of the flag.
///   DEFAULT_VAL - The default value for CMDFLAG.
#define ANALYZER_OPTION(TYPE, NAME, CMDFLAG, DESC, DEFAULT_VAL)
#endif

#ifndef ANALYZER_OPTION_DEPENDS_ON_USER_MODE
/// Create a new analyzer option, but dont generate a method for it in
/// AnalyzerOptions. It's value depends on the option "user-mode".
///
///   TYPE - The type of the option object that will be stored in
///          AnalyzerOptions. This file is expected to be icluded in translation
///          units where AnalyzerOptions.h is included, so types from that
///          header should be used.
///   NAME - The name of the option object.
///   CMDFLAG - The command line flag for the option.
///             (-analyzer-config CMDFLAG=VALUE)
///   DESC - Description of the flag.
///   SHALLOW_VAL - The default value for CMDFLAG, when "user-mode" was set to
///                 "shallow".
///   DEEP_VAL - The default value for CMDFLAG, when "user-mode" was set to
///              "deep".
#define ANALYZER_OPTION_DEPENDS_ON_USER_MODE(TYPE, NAME, CMDFLAG, DESC,        \
                                             SHALLOW_VAL, DEEP_VAL)
#endif

)header";

  std::vector<const ConfigValue *> SortedConfigs;
  SortedConfigs.reserve(Ctx.Configs.size());
  auto ByConfigName = [](const ConfigValue *Lhs, const ConfigValue *Rhs) {
    return Lhs->ConfigName < Rhs->ConfigName;
  };

  for (const auto &Entry : Ctx.Configs)
    SortedConfigs.push_back(Entry.getValue().get());
  llvm::sort(SortedConfigs, ByConfigName);

  for (const ConfigValue *Ptr : SortedConfigs)
    Ptr->print(OS) << "\n";

  OS << R"footer(
#undef ANALYZER_OPTION_DEPENDS_ON_USER_MODE
#undef ANALYZER_OPTION
)footer";
}

static void printAnalyzerOptionCategories(const ParserContext &Ctx,
                                          raw_ostream &OS) {
  std::vector<const ConfigCategory *> SortedCategories;
  SortedCategories.reserve(Ctx.ConfigCategories.size());
  auto ByDisplayOrder = [](const auto *Lhs, const auto *Rhs) {
    return Lhs->DisplayOrder < Rhs->DisplayOrder;
  };

  for (const auto &Entry : Ctx.ConfigCategories)
    SortedCategories.push_back(&Entry.getValue());
  llvm::sort(SortedCategories, ByDisplayOrder);

  for (const auto *Cat : SortedCategories)
    OS << Cat->DisplayName << "\n";
}

void clang::EmitClangSAConfigs(RecordKeeper &Records, raw_ostream &OS) {
  ParserContext Ctx;
  parseConfigCategories(Records, Ctx);
  parseConfigValues(Records, Ctx);
  performSemanticChecks(Records, Ctx);
  printAnalyzerOptions(Ctx, OS);
  printAnalyzerOptionCategories(Ctx, OS);
}

// Invoke with:
// ./build/release/bin/clang-tblgen -gen-clang-sa-configs Foo.td
