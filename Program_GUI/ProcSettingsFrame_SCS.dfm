object frmProcSettingsFrame_SCS: TfrmProcSettingsFrame_SCS
  Left = 0
  Top = 0
  Width = 561
  Height = 714
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlBackground: TPanel
    Left = 0
    Top = 0
    Width = 561
    Height = 714
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = True
    TabOrder = 0
    object lblPresets: TLabel
      Left = 0
      Top = 8
      Width = 275
      Height = 13
      Caption = 'SCS processing settings presets (select preset to load it):'
    end
    object gbEntries: TGroupBox
      Tag = 1100
      Left = 0
      Top = 112
      Width = 561
      Height = 49
      Caption = 'Entries'
      TabOrder = 0
      OnMouseMove = GroupBoxMouseMove
      object cbIgnoreCRC32: TCheckBox
        Tag = 1101
        Left = 8
        Top = 24
        Width = 89
        Height = 17
        Caption = 'Ignore CRC32'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbIgnoreCompressionFlag: TCheckBox
        Tag = 1102
        Left = 192
        Top = 24
        Width = 137
        Height = 17
        Caption = 'Ignore compression flag'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbIgnoreDictID: TCheckBox
        Tag = 1103
        Left = 376
        Top = 24
        Width = 121
        Height = 17
        Caption = 'Ignore dictionary ID'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
    end
    object gbPathResolve: TGroupBox
      Tag = 1200
      Left = 0
      Top = 168
      Width = 561
      Height = 545
      Caption = 'Path resolving'
      TabOrder = 1
      OnMouseMove = GroupBoxMouseMove
      object lblCustomPaths: TLabel
        Left = 8
        Top = 48
        Width = 162
        Height = 13
        Caption = 'Custom paths (one path per line):'
      end
      object lblHelpArchives: TLabel
        Left = 8
        Top = 208
        Width = 268
        Height = 13
        Caption = 'Help archives (one archive per line, full absolute paths):'
      end
      object cbAssumeCityHash: TCheckBox
        Tag = 1201
        Left = 8
        Top = 24
        Width = 113
        Height = 17
        Caption = 'Assume CITY hash'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbUsePredefinedPaths: TCheckBox
        Tag = 1202
        Left = 192
        Top = 24
        Width = 129
        Height = 17
        Caption = 'Use predefined paths'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbExtractUnresolvedEntries: TCheckBox
        Tag = 1203
        Left = 376
        Top = 24
        Width = 153
        Height = 17
        Caption = 'Extract unresolved entries'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object meCustomPaths: TMemo
        Tag = 1204
        Left = 8
        Top = 64
        Width = 545
        Height = 137
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 3
        WordWrap = False
        OnKeyPress = meCustomPathsKeyPress
        OnMouseMove = OptionMouseMove
      end
      object meHelpArchives: TMemo
        Tag = 1205
        Left = 8
        Top = 224
        Width = 545
        Height = 137
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
        OnKeyPress = meHelpArchivesKeyPress
        OnMouseMove = OptionMouseMove
      end
      object btnHelpArchivesMenu: TButton
        Left = 528
        Top = 203
        Width = 25
        Height = 21
        Caption = '6'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = btnHelpArchivesMenuClick
      end
      object gbContentParsing: TGroupBox
        Left = 8
        Top = 368
        Width = 545
        Height = 81
        Caption = 'Content parsing'
        TabOrder = 6
        object lblHint1: TLabel
          Left = 2
          Top = 15
          Width = 541
          Height = 64
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          Caption = 'Not implemented'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
        end
      end
      object gbBruteForce: TGroupBox
        Left = 8
        Top = 456
        Width = 545
        Height = 81
        Caption = 'Brute force resolve'
        TabOrder = 7
        object lblHint2: TLabel
          Left = 2
          Top = 15
          Width = 541
          Height = 64
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          Caption = 'Not implemented'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
        end
      end
    end
    object gbGeneral: TGroupBox
      Left = 0
      Top = 56
      Width = 561
      Height = 49
      Caption = 'General'
      TabOrder = 2
      OnMouseMove = GroupBoxMouseMove
      object cbEntryTabInMem: TCheckBox
        Tag = 1001
        Left = 8
        Top = 24
        Width = 169
        Height = 17
        Caption = 'Process entry table in memory'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
    end
    object cmbPresets: TComboBox
      Left = 0
      Top = 24
      Width = 561
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = cmbPresetsChange
    end
  end
  object pmHelpArchivesMenu: TPopupMenu
    Left = 464
    Top = 368
    object mi_HAM_Browse: TMenuItem
      Caption = 'Browse for help archives...'
      OnClick = mi_HAM_BrowseClick
    end
    object mi_HAM_N1: TMenuItem
      Caption = '-'
    end
    object mi_HAM_ETS2: TMenuItem
      Caption = 'Euro Truck Simulator 2 files'
      OnClick = LoadGameFiles
    end
    object mi_HAM_ATS: TMenuItem
      Caption = 'American Truck Simulator files'
      OnClick = LoadGameFiles
    end
  end
  object diaHelpArchivesOpen: TOpenDialog
    Filter = 
      'All supported files (*.scs; *.zip)|*.scs;*.zip|ZIP archive (*.zi' +
      'p)|*.zip|SCS mod archive (*.scs)|*.scs|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 496
    Top = 368
  end
end
