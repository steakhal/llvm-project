//===- ConfigValues.h - Classes for representing SA configs -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// All Static Analyzer config options are represented by the subclasses of the
/// abstract `ConfigValue` class.
/// The abstract base class defines the metadata common across all config
/// values, while the derived classes can add further structural data, and
/// semantics.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CONFIGVALUES_H
#define LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CONFIGVALUES_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

#include <memory>
#include <string>
#include <vector>

namespace llvm {
class Record;
class RecordKeeper;
} // namespace llvm

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
    Boolean,
    Enum,
    Int,
    String,
    UserModeDependentEnum,
    UserModeDependentInt,
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
  static bool classof(const ConfigValue *C);

  const llvm::StringRef DefaultValue;
};

struct EnumConfigValue final : ConfigValue {
  EnumConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~EnumConfigValue() override = default;
  static bool classof(const ConfigValue *C);

  const llvm::StringRef EnumName;
  const std::vector<llvm::StringRef> Options;
  const llvm::StringRef DefaultValue;
};

struct BooleanConfigValue final : ConfigValue {
  BooleanConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~BooleanConfigValue() override = default;
  static bool classof(const ConfigValue *C);

  const bool DefaultValue;
};

struct IntConfigValue final : ConfigValue {
  IntConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~IntConfigValue() override = default;
  static bool classof(const ConfigValue *C);

  const int64_t Min;
  const llvm::Optional<int64_t> Max;
  const int64_t DefaultValue;
};

struct UserModeDependentIntConfigValue final : ConfigValue {
  UserModeDependentIntConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~UserModeDependentIntConfigValue() override = default;
  static bool classof(const ConfigValue *C);

  const int64_t ShallowMin;
  const int64_t DeepMin;
  const int64_t ShallowDefaultValue;
  const int64_t DeepDefaultValue;
};

struct UserModeDependentEnumConfigValue final : ConfigValue {
  UserModeDependentEnumConfigValue(llvm::Record *R, const ParserContext &Ctx);
  ~UserModeDependentEnumConfigValue() override = default;
  static bool classof(const ConfigValue *C);

  const llvm::StringRef EnumName;
  const std::vector<llvm::StringRef> Options;
  const llvm::StringRef ShallowDefaultValue;
  const llvm::StringRef DeepDefaultValue;
};

template <typename ImplClass, typename RetTy = void> struct ConfigValueVisitor {
  RetTy Visit(const ConfigValue *C) {
    assert(C && "Should not be null.");

    auto *Self = static_cast<ImplClass *>(this);
    using Kind = ConfigValue::ConfigKind;
    using llvm::cast;

    switch (C->Kind) {
    case Kind::Boolean:
      return Self->visit(cast<BooleanConfigValue>(C));
    case Kind::Enum:
      return Self->visit(cast<EnumConfigValue>(C));
    case Kind::Int:
      return Self->visit(cast<IntConfigValue>(C));
    case Kind::String:
      return Self->visit(cast<StringConfigValue>(C));
    case Kind::UserModeDependentEnum:
      return Self->visit(cast<UserModeDependentEnumConfigValue>(C));
    case Kind::UserModeDependentInt:
      return Self->visit(cast<UserModeDependentIntConfigValue>(C));
    }
    llvm_unreachable("Unknown ConfigValue kind!");
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
