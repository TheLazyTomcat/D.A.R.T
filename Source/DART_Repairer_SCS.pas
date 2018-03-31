unit DART_Repairer_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  AuxTypes,
  DART_Common, DART_ProcessingSettings, DART_Format_SCS, DART_Repairer;

type
  TDARTRepairer_SCS = class(TDARTRepairer)
  protected
    fProcessingSettings:  TDART_PS_SCS;
    fArchiveStructure:    TDART_SCS_ArchiveStructure;
    // initialization methods
    //procedure InitializeProcessingSettings; override;
    //procedure InitializeData; override;
    //procedure InitializeProgress; override;
    // methods for content parsing
    //Function IndexOfEntry(const EntryFileName: AnsiString): Integer; override;
    //Function GetEntryData(EntryIndex: Integer; out Data: Pointer; out Size: TMemSize): Boolean; override;
    // processing methods
    //procedure ArchiveProcessing; override;
    // scs specific routines
  public
    //class Function GetMethodNameFromIndex(MethodIndex: Integer): String; override;
    //constructor Create(PauseControlObject: TDARTPauseObject; ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    //Function GetAllKnownPaths(var KnownPaths: TDART_KnownPaths): Integer; override;
    property ArchiveStructure: TDART_SCS_ArchiveStructure read fArchiveStructure;
  end;

  TDARTRepairer_SCS_ProcessingBase = class(TDARTRepairer_SCS)
  end;
  
implementation

end.
