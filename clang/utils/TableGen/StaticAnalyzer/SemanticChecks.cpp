//===- SemanticChecks.cpp                                               ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SemanticChecks.h"
#include "ConfigValues.h"

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

void ento::performSemanticChecks(RecordKeeper &Records,
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
