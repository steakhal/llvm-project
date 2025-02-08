//===- AnalysisExtension.h --------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ModuleFileExtension.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Bitstream/BitstreamReader.h"

namespace clang {

/// A module file extension used for testing purposes.
class AnalysisExtension final
    : public llvm::RTTIExtends<AnalysisExtension, ModuleFileExtension> {
  static constexpr llvm::StringLiteral BlockName = "analysis-extension";
  static constexpr llvm::StringLiteral UserInfo = "some user info?";
  static constexpr auto MajorVersion = 1;
  static constexpr auto MinorVersion = 1;

public:
  static char ID;

  AnalysisExtension() { llvm::errs() << "AnalysisExtension was created\n"; }
  ~AnalysisExtension() { llvm::errs() << "AnalysisExtension was destroyed\n"; }

  ModuleFileExtensionMetadata getExtensionMetadata() const override {
    return {BlockName.str(), MajorVersion, MinorVersion, UserInfo.str()};
  }

  void hashExtension(ExtensionHashBuilder &HBuilder) const override {
    HBuilder.add(BlockName);
    HBuilder.add(MajorVersion);
    HBuilder.add(MinorVersion);
    HBuilder.add(UserInfo);
  }

  std::unique_ptr<ModuleFileExtensionWriter>
  createExtensionWriter(ASTWriter &Writer) override;

  std::unique_ptr<ModuleFileExtensionReader>
  createExtensionReader(const ModuleFileExtensionMetadata &Metadata,
                        ASTReader &Reader, serialization::ModuleFile &Mod,
                        const llvm::BitstreamCursor &Stream) override;

  std::string str() const {
    std::string Buffer;
    llvm::raw_string_ostream OS(Buffer);
    OS << BlockName << ": v" << MajorVersion << "." << MinorVersion
       << ", user info: " << UserInfo;
    return Buffer;
  }
};

} // namespace clang
