//===- ConfigValues.cpp - Classes for representing SA configs ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// This file contains the definitions of the member functions for the
/// `ConfigValue` class hierachy. In addition to those, it also defines the
/// `parseClangSATablegenFile()` function for parsing these config options.
//
//===----------------------------------------------------------------------===//

#include "ConfigValues.h"
#include "SemanticChecks.h"

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"

#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace clang;
using namespace ento;

/// Utilities

static void ensureUniqueValues(const std::vector<StringRef> &List,
                               SMLoc ListLoc) {
  StringSet<> Uniques;
  bool Inserted;
  for (StringRef Item : List) {
    std::tie(std::ignore, Inserted) = Uniques.insert(Item);
    if (!Inserted)
      PrintFatalError(ListLoc,
                      "\"" + Item + "\" appears more than once in the list!\n");
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

/// Constructors

ConfigCategory::ConfigCategory(Record *R, const ParserContext &Ctx)
    : Name(R->getName()), DisplayName(R->getValueAsString("DisplayName")),
      Description(R->getValueAsString("Description")), Loc(R->getLoc().back()) {
}

ConfigValue::ConfigValue(ConfigKind K, Record *R, const ParserContext &Ctx)
    : Kind(K), ConfigName(R->getName()),
      FlagName(R->getValueAsString("FlagName")),
      ShortDescription(R->getValueAsString("ShortDescription")),
      LongDescription(
          R->getValueAsOptionalString("LongDescription").getValueOr("")),
      RelatedConfigs(parseListFieldIfDefined(R, "RelatedConfigs")),
      RelatedCheckers(parseListFieldIfDefined(R, "RelatedCheckers")),
      Category(Ctx.lookupConfigCategory(R->getValueAsDef("Category"))),
      Loc(R->getLoc().back()) {
  assert(R->isSubClassOf("ConfigValue"));
  assert(!ConfigName.empty());

  if (FlagName.empty())
    PrintFatalError(R->getFieldLoc("FlagName"),
                    "`FlagName' should not be empty.");

  if (ShortDescription.empty())
    PrintFatalError(R->getFieldLoc("ShortDescription"),
                    "`ShortDescription' should not be empty.");

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
    : ConfigValue(Boolean, R, Ctx),
      DefaultValue(R->getValueAsBit("DefaultValue")) {}

EnumConfigValue::EnumConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigValue(Enum, R, Ctx), EnumName(R->getValueAsString("EnumName")),
      Options(R->getValueAsListOfStrings("Options")),
      DefaultValue(R->getValueAsString("DefaultValue")) {
  SMLoc ListLoc = R->getFieldLoc("Options");
  SMLoc DefaultValueLoc = R->getFieldLoc("DefaultValue");
  if (Options.size() < 2)
    PrintFatalError(ListLoc,
                    "The `Options' list must have at least two elements!\n");

  if (!is_contained(Options, DefaultValue)) {
    PrintError(ListLoc, "The field named `Options' must contain the value of "
                        "`DefaultValue'!\n");
    PrintFatalNote(DefaultValueLoc,
                   "`DefaultValue' is \"" + DefaultValue + "\"");
  }

  ensureUniqueValues(Options, ListLoc);
}

IntConfigValue::IntConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigValue(Int, R, Ctx), Min(R->getValueAsInt("Min")),
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
                            "equal to the field named `Max'!\n");
    PrintNote(R->getFieldLoc("DefaultValue"),
              "`DefaultValue' is " + std::to_string(DefaultValue));
    PrintFatalNote(R->getFieldLoc("Max"), "`Max' is " + std::to_string(*Max));
  }
}

StringConfigValue::StringConfigValue(Record *R, const ParserContext &Ctx)
    : ConfigValue(String, R, Ctx),
      DefaultValue(R->getValueAsString("DefaultValue")) {}

UserModeDependentEnumConfigValue::UserModeDependentEnumConfigValue(
    Record *R, const ParserContext &Ctx)
    : ConfigValue(UserModeDependentEnum, R, Ctx),
      EnumName(R->getValueAsString("EnumName")),
      Options(R->getValueAsListOfStrings("Options")),
      ShallowDefaultValue(R->getValueAsString("ShallowDefaultValue")),
      DeepDefaultValue(R->getValueAsString("DeepDefaultValue")) {
  SMLoc ListLoc = R->getFieldLoc("Options");
  SMLoc ShallowDefaultValueLoc = R->getFieldLoc("ShallowDefaultValue");
  SMLoc DeepDefaultValueLoc = R->getFieldLoc("DeepDefaultValue");
  if (Options.size() < 2)
    PrintFatalError(ListLoc,
                    "The `Options' list must have at least two elements!\n");

  if (!is_contained(Options, ShallowDefaultValue)) {
    PrintError(ListLoc, "The field named `Options' must contain the value of "
                        "`ShallowDefaultValue'!\n");
    PrintFatalNote(ShallowDefaultValueLoc,
                   "`ShallowDefaultValue' is \"" + ShallowDefaultValue + "\"");
  }

  if (!is_contained(Options, DeepDefaultValue)) {
    PrintError(ListLoc, "The field named `Options' must contain the value of "
                        "`DeepDefaultValue'!\n");
    PrintFatalNote(DeepDefaultValueLoc,
                   "`DeepDefaultValue' is \"" + DeepDefaultValue + "\"");
  }

  ensureUniqueValues(Options, ListLoc);
}

UserModeDependentIntConfigValue::UserModeDependentIntConfigValue(
    Record *R, const ParserContext &Ctx)
    : ConfigValue(UserModeDependentInt, R, Ctx),
      ShallowMin(R->getValueAsInt("ShallowMin")),
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

/// 'classof' implementations

bool BooleanConfigValue::classof(const ConfigValue *C) {
  return C->Kind == Boolean;
}
bool EnumConfigValue::classof(const ConfigValue *C) { return C->Kind == Enum; }
bool IntConfigValue::classof(const ConfigValue *C) { return C->Kind == Int; }
bool StringConfigValue::classof(const ConfigValue *C) {
  return C->Kind == String;
}
bool UserModeDependentEnumConfigValue::classof(const ConfigValue *C) {
  return C->Kind == UserModeDependentEnum;
}
bool UserModeDependentIntConfigValue::classof(const ConfigValue *C) {
  return C->Kind == UserModeDependentInt;
}

/// Other implementations
namespace {
struct PointeeLessThan {
  template <typename T, typename U>
  bool operator()(const T *LHS, const U *RHS) {
    return *LHS < *RHS;
  }
};
} // namespace

bool ento::operator<(const ConfigCategory &LHS, const ConfigCategory &RHS) {
  // Sort by (line,column).
  auto LHSLineAndCol = llvm::SrcMgr.getLineAndColumn(LHS.Loc);
  auto RHSLineAndCol = llvm::SrcMgr.getLineAndColumn(RHS.Loc);
  return LHSLineAndCol < RHSLineAndCol;
}

bool ento::operator<(const ConfigValue &LHS, const ConfigValue &RHS) {
  // Sort by (line,column).
  auto LHSLineAndCol = llvm::SrcMgr.getLineAndColumn(LHS.Loc);
  auto RHSLineAndCol = llvm::SrcMgr.getLineAndColumn(RHS.Loc);
  return LHSLineAndCol < RHSLineAndCol;
}

const ConfigCategory &ParserContext::lookupConfigCategory(Record *R) const {
  StringRef CategoryName = R->getName();
  const auto It = ConfigCategories.find(CategoryName);
  assert(It != ConfigCategories.end());
  return It->getValue();
}

std::vector<const ConfigValue *>
ParserContext::getConfigsInSpellingOrder() const {
  std::vector<const ConfigValue *> SortedConfigs;
  SortedConfigs.reserve(Configs.size());

  for (const auto &Entry : Configs)
    SortedConfigs.push_back(Entry.getValue().get());
  llvm::sort(SortedConfigs, PointeeLessThan{});
  return SortedConfigs;
}
std::vector<const ConfigCategory *>
ParserContext::getConfigCategoriesInSpellingOrder() const {
  std::vector<const ConfigCategory *> SortedCategories;
  SortedCategories.reserve(ConfigCategories.size());
  for (const auto &Entry : ConfigCategories)
    SortedCategories.push_back(&Entry.getValue());

  // Sort by (line,column).
  llvm::sort(SortedCategories, PointeeLessThan{});
  return SortedCategories;
}

static std::unique_ptr<ConfigValue>
parseSingleConfigValue(Record *R, const ParserContext &Ctx,
                       StringRef DirectBaseName) {
  using std::make_unique;
  if ("BooleanConfigValue" == DirectBaseName)
    return make_unique<BooleanConfigValue>(R, Ctx);
  if ("EnumConfigValue" == DirectBaseName)
    return make_unique<EnumConfigValue>(R, Ctx);
  if ("IntConfigValue" == DirectBaseName)
    return make_unique<IntConfigValue>(R, Ctx);
  if ("StringConfigValue" == DirectBaseName)
    return make_unique<StringConfigValue>(R, Ctx);
  if ("UserModeDependentEnumConfigValue" == DirectBaseName)
    return make_unique<UserModeDependentEnumConfigValue>(R, Ctx);
  if ("UserModeDependentIntConfigValue" == DirectBaseName)
    return make_unique<UserModeDependentIntConfigValue>(R, Ctx);

  PrintFatalError(R->getLoc(),
                  "Record `" + DirectBaseName +
                      "' is unhandled by the \"gen-clang-sa-configs\" "
                      "tablegen backend!\n");
}

static void parseConfigCategories(RecordKeeper &Records, ParserContext &Ctx) {
  for (Record *R : Records.getAllDerivedDefinitions("ConfigCategory")) {
    Ctx.ConfigCategories.insert(
        std::make_pair(R->getName(), ConfigCategory{R, Ctx}));
  }
}

static void parseConfigValues(RecordKeeper &Records, ParserContext &Ctx) {
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
        R->getName(), parseSingleConfigValue(R, Ctx, DirectBaseName)));
  }
}

ParserContext ento::parseClangSATablegenFile(RecordKeeper &Records) {
  ParserContext Ctx;
  parseConfigCategories(Records, Ctx);
  parseConfigValues(Records, Ctx);
  performSemanticChecks(Records, Ctx);
  return Ctx;
}
