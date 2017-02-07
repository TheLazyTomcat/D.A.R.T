object frmProcSettingsSCS: TfrmProcSettingsSCS
  Left = 0
  Top = 0
  Width = 561
  Height = 569
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
    Height = 569
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = True
    TabOrder = 0
    object gbEntries: TGroupBox
      Tag = 120
      Left = 0
      Top = 0
      Width = 561
      Height = 49
      Caption = 'Entries'
      TabOrder = 0
      OnMouseMove = GroupBoxMouseMove
      object cbIgnoreCRC32: TCheckBox
        Tag = 121
        Left = 8
        Top = 24
        Width = 89
        Height = 17
        Caption = 'Ignore CRC32'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbIgnoreCompressionFlag: TCheckBox
        Tag = 122
        Left = 192
        Top = 24
        Width = 137
        Height = 17
        Caption = 'Ignore compression flag'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbIgnoreDictID: TCheckBox
        Tag = 123
        Left = 376
        Top = 24
        Width = 121
        Height = 17
        Caption = 'Ignore dictionary ID'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
    end
    object gbPathResolve: TGroupBox
      Tag = 140
      Left = 0
      Top = 56
      Width = 561
      Height = 513
      Caption = 'Path resolve'
      TabOrder = 1
      OnMouseMove = GroupBoxMouseMove
      object lblHelpFiles: TLabel
        Left = 8
        Top = 208
        Width = 226
        Height = 13
        Caption = 'Help files (one file per line, full absolute paths):'
      end
      object lblCustomPaths: TLabel
        Left = 8
        Top = 48
        Width = 162
        Height = 13
        Caption = 'Custom paths (one path per line):'
      end
      object cbAssumeCityHash: TCheckBox
        Tag = 141
        Left = 8
        Top = 24
        Width = 113
        Height = 17
        Caption = 'Assume City hash'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbUsePredefinedPaths: TCheckBox
        Tag = 142
        Left = 192
        Top = 24
        Width = 129
        Height = 17
        Caption = 'Use predefined paths'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbExtractUnresolvedEntries: TCheckBox
        Tag = 143
        Left = 376
        Top = 24
        Width = 153
        Height = 17
        Caption = 'Extract unresolved entries'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object meHelpFiles: TMemo
        Tag = 152
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
        TabOrder = 5
        WordWrap = False
        OnMouseMove = SettingsMouseMove
      end
      object meCustomPaths: TMemo
        Tag = 151
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
        OnMouseMove = SettingsMouseMove
      end
      object btnBrowseHelpFiles: TButton
        Left = 530
        Top = 203
        Width = 24
        Height = 20
        Caption = '6'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = btnBrowseHelpFilesClick
      end
      object gbContentParsing: TGroupBox
        Tag = 160
        Left = 8
        Top = 368
        Width = 545
        Height = 49
        Caption = 'Content parsing'
        TabOrder = 6
        OnMouseMove = GroupBoxMouseMove
        object cbParseHelpFiles: TCheckBox
          Tag = 163
          Left = 384
          Top = 24
          Width = 153
          Height = 17
          Caption = 'Parse content of help files'
          TabOrder = 2
          OnClick = CheckBoxClick
          OnMouseMove = SettingsMouseMove
        end
        object cbParseContent: TCheckBox
          Tag = 161
          Left = 8
          Top = 24
          Width = 129
          Height = 17
          Caption = 'Parse archive content'
          TabOrder = 0
          OnClick = CheckBoxClick
          OnMouseMove = SettingsMouseMove
        end
        object cbParseEverything: TCheckBox
          Tag = 162
          Left = 192
          Top = 24
          Width = 105
          Height = 17
          Caption = 'Parse everything'
          TabOrder = 1
          OnClick = CheckBoxClick
          OnMouseMove = SettingsMouseMove
        end
      end
      object gbBruteForce: TGroupBox
        Tag = 170
        Left = 8
        Top = 424
        Width = 545
        Height = 81
        Caption = 'Brute-force resolve'
        TabOrder = 7
        OnMouseMove = GroupBoxMouseMove
        object lblLengthLimit: TLabel
          Left = 385
          Top = 50
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'Path length limit:'
        end
        object cbMultithread: TCheckBox
          Tag = 173
          Left = 384
          Top = 24
          Width = 137
          Height = 17
          Caption = 'Multithreaded operation'
          TabOrder = 2
          OnClick = CheckBoxClick
          OnMouseMove = SettingsMouseMove
        end
        object cbAllowBruteForce: TCheckBox
          Tag = 171
          Left = 8
          Top = 24
          Width = 145
          Height = 17
          Caption = 'Allow brute-force resolve'
          TabOrder = 0
          OnClick = CheckBoxClick
          OnMouseMove = SettingsMouseMove
        end
        object cbLimitedAlphabet: TCheckBox
          Tag = 174
          Left = 8
          Top = 48
          Width = 105
          Height = 17
          Caption = 'Limited alphabet'
          TabOrder = 3
          OnClick = CheckBoxClick
          OnMouseMove = SettingsMouseMove
        end
        object cbMultiSearch: TCheckBox
          Tag = 172
          Left = 192
          Top = 24
          Width = 129
          Height = 17
          Caption = 'Multiple entries search'
          TabOrder = 1
          OnClick = CheckBoxClick
          OnMouseMove = SettingsMouseMove
        end
        object seLengthLimit: TSpinEdit
          Tag = 175
          Left = 472
          Top = 47
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnMouseMove = SettingsMouseMove
        end
      end
    end
  end
  object pmHelpFiles: TPopupMenu
    Left = 464
    Top = 256
    object miHelpFiles_Browse: TMenuItem
      Caption = 'Browse for help files...'
      OnClick = miHelpFiles_BrowseClick
    end
    object miHelpFiles_N1: TMenuItem
      Caption = '-'
      Visible = False
    end
    object miHelpFiles_ETS2: TMenuItem
      Caption = 'Euro Truck Simulator 2 files'
      Visible = False
      OnClick = LoadGameFiles
    end
    object miHelpFiles_ATS: TMenuItem
      Caption = 'American Truck Simulator files'
      Visible = False
      OnClick = LoadGameFiles
    end
  end
  object diaHelpFilesOpen: TOpenDialog
    Filter = 
      'All supported files (*.scs; *.zip)|*.scs;*.zip|ZIP archive (*.zi' +
      'p)|*.zip|SCS mod archive (*.scs)|*.scs|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 496
    Top = 256
  end
end
