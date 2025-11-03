//===- USRMapper.h ---------------------------------------------- -*- C++ --*-//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"

#include <optional>
#include <string>

namespace clang {
class Decl;
class DeclContext;

using USRString = std::string;

class USRMapper {
public:
  USRMapper();
  void traverseDecl(const Decl *D);

  // May return null.
  const Decl *getDeclForUSR(llvm::StringRef USR) const;
  std::optional<USRString> getUSRForDecl(const Decl *D) const;

private:
  llvm::DenseMap<const Decl *, USRString> DeclToUSR;
  llvm::DenseMap<USRString, const Decl *> USRToDecl;
};

} // namespace clang
