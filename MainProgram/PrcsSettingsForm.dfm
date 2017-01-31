object fPrcsSettingsForm: TfPrcsSettingsForm
  Left = 242
  Top = 165
  BorderStyle = bsDialog
  Caption = 'Processing settings'
  ClientHeight = 544
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
  object lblSettingDescription: TLabel
    Left = 616
    Top = 8
    Width = 93
    Height = 13
    Caption = 'Setting description:'
  end
  object btnAccept: TButton
    Left = 808
    Top = 512
    Width = 97
    Height = 25
    Caption = 'Accept'
    TabOrder = 4
    OnClick = btnAcceptClick
  end
  object btnClose: TButton
    Left = 912
    Top = 512
    Width = 97
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object grbCommonSettings: TGroupBox
    Left = 8
    Top = 8
    Width = 601
    Height = 185
    Caption = 'Common settings'
    TabOrder = 0
    OnMouseMove = GroupBoxMouseMove
    object bvlGeneralhorSplit: TBevel
      Left = 8
      Top = 150
      Width = 585
      Height = 9
      Shape = bsTopLine
    end
    object vblGeneralHorSplit_File: TBevel
      Left = 8
      Top = 70
      Width = 585
      Height = 9
      Shape = bsTopLine
    end
    object lblFile: TLabel
      Tag = 1
      Left = 72
      Top = 20
      Width = 26
      Height = 13
      Caption = 'lblFile'
      Color = clBtnFace
      Constraints.MaxWidth = 521
      ParentColor = False
      OnMouseMove = SettingsMouseMove
    end
    object lblFileCpt: TLabel
      Left = 43
      Top = 20
      Width = 22
      Height = 13
      Alignment = taRightJustify
      Caption = 'File:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblFileTypeCpt: TLabel
      Left = 14
      Top = 44
      Width = 51
      Height = 13
      Alignment = taRightJustify
      Caption = 'File type:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblFileType: TLabel
      Tag = 2
      Left = 72
      Top = 44
      Width = 50
      Height = 13
      Caption = 'lblFileType'
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreFileSignature: TCheckBox
      Tag = 10
      Left = 8
      Top = 160
      Width = 121
      Height = 17
      Caption = 'Ignore file signature'
      TabOrder = 6
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
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
      OnMouseMove = SettingsMouseMove
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
      OnMouseMove = SettingsMouseMove
    end
    object lbleTarget: TLabeledEdit
      Tag = 8
      Left = 8
      Top = 120
      Width = 560
      Height = 21
      EditLabel.Width = 36
      EditLabel.Height = 13
      EditLabel.Caption = 'Target:'
      TabOrder = 4
      OnChange = lbleTargetChange
      OnMouseMove = SettingsMouseMove
    end
    object btnBrowse: TButton
      Tag = 9
      Left = 568
      Top = 120
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = btnBrowseClick
    end
    object cbInMemoryProcessing: TCheckBox
      Tag = 11
      Left = 192
      Top = 160
      Width = 129
      Height = 17
      Caption = 'In memory processing'
      TabOrder = 7
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreErroneousEntries: TCheckBox
      Tag = 12
      Left = 376
      Top = 160
      Width = 145
      Height = 17
      Caption = 'Ignore erroneous entries'
      TabOrder = 8
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbForceFileType: TCheckBox
      Tag = 3
      Left = 400
      Top = 42
      Width = 97
      Height = 17
      Caption = 'Force file type:'
      TabOrder = 0
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cmbForcedFileType: TComboBox
      Tag = 4
      Left = 496
      Top = 40
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'ZIP'
      OnChange = cmbForcedFileTypeChange
      Items.Strings = (
        'ZIP'
        'SCS#')
    end
    object rbConvert: TRadioButton
      Tag = 7
      Left = 216
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Convert archive'
      TabOrder = 9
      OnClick = RepairMethodClick
      OnMouseMove = SettingsMouseMove
    end
  end
  object meSettingDescription: TMemo
    Left = 616
    Top = 24
    Width = 393
    Height = 481
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Move cursor over specific setting to see its '
      'description.')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnDefault: TButton
    Left = 616
    Top = 512
    Width = 137
    Height = 25
    Caption = 'Load default settings...'
    TabOrder = 3
    OnClick = btnDefaultClick
  end
  object grbArchiveSettings: TGroupBox
    Left = 8
    Top = 200
    Width = 601
    Height = 337
    Caption = 'Archive settings'
    TabOrder = 1
    object scbArchiveSettings: TScrollBox
      Left = 8
      Top = 16
      Width = 585
      Height = 313
      HorzScrollBar.Visible = False
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      BorderStyle = bsNone
      TabOrder = 0
      inline frmProcSettingsSCS: TfrmProcSettingsSCS
        Left = 0
        Top = 0
        Width = 561
        Height = 473
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      inline frmProcSettingsZIP: TfrmProcSettingsZIP
        Left = 0
        Top = 0
        Width = 561
        Height = 473
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
  end
  object diaSaveDialog: TSaveDialog
    DefaultExt = '.scs'
    Filter = 
      'All supported files (*.scs; *.zip)|*.scs;*.zip|ZIP archive (*.zi' +
      'p)|*.zip|SCS mod archive (*.scs)|*.scs|All files (*.*)|*.*'
    Left = 976
  end
end
