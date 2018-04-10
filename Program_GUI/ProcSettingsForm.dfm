object fProcSettingsForm: TfProcSettingsForm
  Left = 252
  Top = 50
  BorderStyle = bsDialog
  Caption = 'Archive processing settings'
  ClientHeight = 640
  ClientWidth = 1016
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblOptionDescription: TLabel
    Left = 616
    Top = 8
    Width = 91
    Height = 13
    Caption = 'Option description:'
  end
  object bvlMainHorSplit: TBevel
    Left = 0
    Top = 600
    Width = 1017
    Height = 9
    Shape = bsTopLine
  end
  object meOptionDecription: TMemo
    Left = 616
    Top = 24
    Width = 393
    Height = 569
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Move cursor over specific option to see its '
      'description.')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyPress = meOptionDecriptionKeyPress
  end
  object brCommonSettings: TGroupBox
    Left = 8
    Top = 8
    Width = 601
    Height = 185
    Caption = 'Common settings'
    TabOrder = 1
    OnMouseMove = GroupBoxMouseMove
    object lblArchiveFileCpt: TLabel
      Left = 23
      Top = 20
      Width = 66
      Height = 13
      Alignment = taRightJustify
      Caption = 'Archive file:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblArchiveTypeCpt: TLabel
      Left = 14
      Top = 44
      Width = 75
      Height = 13
      Alignment = taRightJustify
      Caption = 'Archive type:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblArchiveType: TLabel
      Tag = 2
      Left = 96
      Top = 44
      Width = 70
      Height = 13
      Caption = 'lblArchiveType'
      OnMouseMove = OptionMouseMove
    end
    object lblArchiveFile: TLabel
      Tag = 1
      Left = 96
      Top = 20
      Width = 62
      Height = 13
      Caption = 'lblArchiveFile'
      Constraints.MaxWidth = 497
      OnMouseMove = OptionMouseMove
    end
    object bvlHorSplitFile: TBevel
      Left = 8
      Top = 70
      Width = 585
      Height = 9
      Shape = bsTopLine
    end
    object bvlHorSplitTarget: TBevel
      Left = 8
      Top = 150
      Width = 585
      Height = 9
      Shape = bsTopLine
    end
    object lblConvertTo: TLabel
      Left = 433
      Top = 82
      Width = 56
      Height = 13
      Alignment = taRightJustify
      Caption = 'Convert to:'
    end
    object cbForceArchiveType: TCheckBox
      Tag = 3
      Left = 376
      Top = 42
      Width = 121
      Height = 17
      Caption = 'Force archive type:'
      TabOrder = 0
      OnClick = CheckBoxClick
      OnMouseMove = OptionMouseMove
    end
    object cmbForcedArchiveType: TComboBox
      Tag = 4
      Left = 496
      Top = 40
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = cmbForcedArchiveTypeChange
    end
    object rbRebuild: TRadioButton
      Tag = 5
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Rebuild archive'
      TabOrder = 2
      OnClick = RepairMethodClick
      OnMouseMove = OptionMouseMove
    end
    object rbExtract: TRadioButton
      Tag = 6
      Left = 112
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Extract archive'
      TabOrder = 3
      OnClick = RepairMethodClick
      OnMouseMove = OptionMouseMove
    end
    object rbConvert: TRadioButton
      Tag = 7
      Left = 216
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Convert archive'
      TabOrder = 4
      OnClick = RepairMethodClick
      OnMouseMove = OptionMouseMove
    end
    object lbleTarget: TLabeledEdit
      Tag = 9
      Left = 8
      Top = 120
      Width = 560
      Height = 21
      EditLabel.Width = 36
      EditLabel.Height = 13
      EditLabel.Caption = 'Target:'
      TabOrder = 6
      OnChange = lbleTargetChange
      OnMouseMove = OptionMouseMove
    end
    object btnBrowseTarget: TButton
      Tag = -1
      Left = 568
      Top = 120
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 7
      OnClick = btnBrowseTargetClick
    end
    object cbIgnoreArchiveSignature: TCheckBox
      Tag = 10
      Left = 8
      Top = 160
      Width = 145
      Height = 17
      Caption = 'Ignore archive signature'
      TabOrder = 8
      OnClick = CheckBoxClick
      OnMouseMove = OptionMouseMove
    end
    object cbInMemoryProcessing: TCheckBox
      Tag = 11
      Left = 192
      Top = 160
      Width = 129
      Height = 17
      Caption = 'In-memory processing'
      TabOrder = 9
      OnClick = CheckBoxClick
      OnMouseMove = OptionMouseMove
    end
    object cbIgnoreErroneousEntries: TCheckBox
      Tag = 12
      Left = 376
      Top = 160
      Width = 145
      Height = 17
      Caption = 'Ignore erroneous entries'
      TabOrder = 10
      OnClick = CheckBoxClick
      OnMouseMove = OptionMouseMove
    end
    object cmbConvertTo: TComboBox
      Tag = 8
      Left = 496
      Top = 78
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
    end
  end
  object gbArchiveSettings: TGroupBox
    Left = 8
    Top = 200
    Width = 601
    Height = 393
    Caption = 'Archive settings'
    TabOrder = 2
    object scbArchiveSettings: TScrollBox
      Left = 8
      Top = 16
      Width = 585
      Height = 369
      HorzScrollBar.Visible = False
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      BorderStyle = bsNone
      ParentBackground = True
      TabOrder = 0
    end
  end
  object btnSaveSettings: TButton
    Left = 8
    Top = 608
    Width = 161
    Height = 25
    Caption = 'Save processing settings...'
    TabOrder = 3
    OnClick = btnSaveSettingsClick
  end
  object btnLoadSettings: TButton
    Left = 176
    Top = 608
    Width = 161
    Height = 25
    Caption = 'Load processing settings...'
    TabOrder = 4
    OnClick = btnLoadSettingsClick
  end
  object btnDefaultSettings: TButton
    Left = 424
    Top = 608
    Width = 177
    Height = 25
    Caption = 'Load default processing settings'
    TabOrder = 5
    OnClick = btnDefaultSettingsClick
  end
  object btnAccept: TButton
    Left = 808
    Top = 608
    Width = 97
    Height = 25
    Caption = 'Accept'
    TabOrder = 6
    OnClick = btnAcceptClick
  end
  object btnClose: TButton
    Left = 912
    Top = 608
    Width = 97
    Height = 25
    Caption = 'Close'
    TabOrder = 7
    OnClick = btnCloseClick
  end
  object diaTargetSave: TSaveDialog
    DefaultExt = #39'.scs'#39
    Filter = 
      'All supported files (*.scs; *.zip)|*.scs;*.zip|ZIP archive (*.zi' +
      'p)|*.zip|SCS mod archive (*.scs)|*.scs|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 912
  end
  object diaProcSettOpen: TOpenDialog
    Filter = 'INI files (*.ini)|*.ini|All files (*.*)|*.*'
    Left = 944
  end
  object diaProcSettSave: TSaveDialog
    DefaultExt = '.ini'
    Filter = 'INI files (*.ini)|*.ini|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 976
  end
end
