//===- ConfigValues.h                                                   ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CONFIGVALUES_H
#define LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CONFIGVALUES_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/TableGen/Record.h"

#include <memory>
#include <string>
#include <vector>

namespace clang {
namespace ento {

struct ParserContext;
struct ConfigValue;

struct ConfigCategory {
  ConfigCategory(llvm::Record *R, const ParserContext &Ctx);

  const int64_t DisplayOrder;
  const llvm::StringRef Name;
  const llvm::StringRef DisplayName;
  const llvm::StringRef Description;
};

struct ParserContext {
  llvm::StringMap<std::unique_ptr<ConfigValue>> Configs;
  llvm::StringMap<ConfigCategory> ConfigCategories;

  const ConfigCategory &lookupConfigCategory(llvm::Record *R) const;
};

struct ConfigValue {
  enum ConfigKind : unsigned {
    BooleanKind,
    EnumKind,
    IntKind,
    StringKind,
    UserModeDependentEnumKind,
    UserModeDependentIntKind,
  };
  ConfigValue(ConfigKind K, llvm::Record *R, const ParserContext &Ctx);
  virtual ~ConfigValue() = default;

  const ConfigKind Kind;
  const llvm::StringRef ConfigName;
  const llvm::StringRef FlagName;
  const llvm::StringRef ShortDescription;
  const llvm::StringRef LongDescription;
  const std::vector<llvm::StringRef> RelatedConfigs;
  const std::vector<llvm::StringRef> RelatedCheckers;
  const ConfigCategory &Category;
};

struct StringConfigValue final : ConfigValue {
  StringConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~StringConfigValue() override = default;

  const llvm::StringRef DefaultValue;
};

struct EnumConfigValue final : ConfigValue {
  EnumConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~EnumConfigValue() override = default;

  const llvm::StringRef EnumName;
  const std::vector<llvm::StringRef> Options;
  const llvm::StringRef DefaultValue;
};

struct BooleanConfigValue final : ConfigValue {
  BooleanConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~BooleanConfigValue() override = default;

  const bool DefaultValue;
};

struct IntConfigValue final : ConfigValue {
  IntConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~IntConfigValue() override = default;

  const int64_t Min;
  const llvm::Optional<int64_t> Max;
  const int64_t DefaultValue;
};

struct UserModeDependentIntConfigValue final : ConfigValue {
  UserModeDependentIntConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~UserModeDependentIntConfigValue() override = default;

  const int64_t ShallowMin;
  const int64_t DeepMin;
  const int64_t ShallowDefaultValue;
  const int64_t DeepDefaultValue;
};

struct UserModeDependentEnumConfigValue final : ConfigValue {
  UserModeDependentEnumConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~UserModeDependentEnumConfigValue() override = default;

  const llvm::StringRef EnumName;
  const std::vector<llvm::StringRef> Options;
  const llvm::StringRef ShallowDefaultValue;
  const llvm::StringRef DeepDefaultValue;
};

template <typename ImplClass, typename RetTy = void> struct ConfigValueVisitor {
  RetTy Visit(const ConfigValue *C) {
    auto *Self = static_cast<ImplClass *>(this);
    using Kind = ConfigValue::ConfigKind;
    switch (C->Kind) {
    case Kind::BooleanKind:
      return Self->visit(static_cast<const BooleanConfigValue *>(C));
    case Kind::EnumKind:
      return Self->visit(static_cast<const EnumConfigValue *>(C));
    case Kind::IntKind:
      return Self->visit(static_cast<const IntConfigValue *>(C));
    case Kind::StringKind:
      return Self->visit(static_cast<const StringConfigValue *>(C));
    case Kind::UserModeDependentEnumKind:
      return Self->visit(
          static_cast<const UserModeDependentEnumConfigValue *>(C));
    case Kind::UserModeDependentIntKind:
      return Self->visit(
          static_cast<const UserModeDependentIntConfigValue *>(C));
    }
  }
  RetTy visit(const BooleanConfigValue *) { return RetTy{}; }
  RetTy visit(const EnumConfigValue *) { return RetTy{}; }
  RetTy visit(const IntConfigValue *) { return RetTy{}; }
  RetTy visit(const StringConfigValue *) { return {}; }
  RetTy visit(const UserModeDependentEnumConfigValue *) { return RetTy{}; }
  RetTy visit(const UserModeDependentIntConfigValue *) { return RetTy{}; }
};

ParserContext parseClangSATablegenFile(llvm::RecordKeeper &Records);

} // namespace ento
} // namespace clang

#endif // LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CONFIGVALUES_H
