{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_ZIP;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes,
  DART_Repairer;

const
  FileSignature_ZIP = UInt32($04034b50);

type
  TRepairer_ZIP = class(TRepairer);

  TRepairer_ZIP_Rebuild = class(TRepairer_ZIP);
  TRepairer_ZIP_Extract = class(TRepairer_ZIP);

implementation

end.
