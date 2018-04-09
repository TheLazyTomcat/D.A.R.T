{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit DART_Repairer_SCS_Convert;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  DART_Repairer_SCS;

{===============================================================================
--------------------------------------------------------------------------------
                           TDARTRepairer_SCS_Convert
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS_Convert - class declaration
===============================================================================}
type
  TDARTRepairer_SCS_Convert = class(TDARTRepairer_SCS_ProcessingBase)
  protected
    fConvertedArchiveStream:  TStream;
    procedure ConvertArchiveStructure; virtual; abstract;
    procedure ConvertArchive; virtual; abstract;
    procedure ArchiveProcessing; override;
  end;

implementation

{===============================================================================
--------------------------------------------------------------------------------
                           TDARTRepairer_SCS_Convert
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TDARTRepairer_SCS_Convert - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TDARTRepairer_SCS_Convert - protected methods
-------------------------------------------------------------------------------}

procedure TDARTRepairer_SCS_Convert.ArchiveProcessing;
begin
inherited;
ConvertArchiveStructure;
ConvertArchive;
end;

end.
