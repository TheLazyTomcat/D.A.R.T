object fMainForm: TfMainForm
  Left = 438
  Top = 115
  BorderStyle = bsSingle
  Caption = 'D.A.R.T - Damaged Archives Repair Tool'
  ClientHeight = 486
  ClientWidth = 832
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    832
    486)
  PixelsPerInch = 96
  TextHeight = 13
  object lblArchiveList: TLabel
    Left = 8
    Top = 8
    Width = 125
    Height = 13
    Caption = 'Archives to be processed:'
  end
  object bvlProgressSplit: TBevel
    Left = 8
    Top = 376
    Width = 817
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object lblOverallProgress: TLabel
    Left = 8
    Top = 384
    Width = 83
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Overall progress:'
  end
  object lblArchiveProgress: TLabel
    Left = 8
    Top = 424
    Width = 85
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Archive progress:'
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 467
    Width = 832
    Height = 19
    Panels = <
      item
        Alignment = taRightJustify
        Width = 50
      end>
  end
  object lvArchiveList: TListView
    Left = 8
    Top = 24
    Width = 817
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Width = 25
      end
      item
        Caption = 'Archive file name'
        Width = 280
      end
      item
        Caption = 'Archive size'
        Width = 75
      end
      item
        Caption = 'Archive type'
        Width = 100
      end
      item
        Caption = 'Repair method'
        Width = 155
      end
      item
        Caption = 'Archive processing status'
        Width = 140
      end>
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = pmArchiveListMenu
    ShowHint = True
    SmallImages = ilArchiveList
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvArchiveListDblClick
  end
  object btnProcessing: TButton
    Left = 8
    Top = 344
    Width = 817
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'btnProcessing'
    TabOrder = 1
    OnClick = btnProcessingClick
  end
  object pbOverallProgress: TProgressBar
    Left = 8
    Top = 400
    Width = 817
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
  end
  object pbArchiveProgress: TProgressBar
    Left = 8
    Top = 440
    Width = 817
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
  end
  object oXPManifest: TXPManifest
    Left = 792
  end
  object pmArchiveListMenu: TPopupMenu
    OnPopup = pmArchiveListMenuPopup
    Left = 760
    object pmiAL_Add: TMenuItem
      Caption = 'Add archives...'
      ShortCut = 45
      OnClick = pmiAL_AddClick
    end
    object pmiAL_Remove: TMenuItem
      Caption = 'Remove selected archives'
      ShortCut = 46
      OnClick = pmiAL_RemoveClick
    end
    object pmiAL_Clear: TMenuItem
      Caption = 'Clear entire list'
      ShortCut = 8238
      OnClick = pmiAL_ClearClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmiAL_ProcessingSettings: TMenuItem
      Caption = 'Archive processing settings...'
      OnClick = pmiAL_ProcessingSettingsClick
    end
    object pmiAL_ResultInfo: TMenuItem
      Caption = 'Result information...'
      OnClick = pmiAL_ResultInfoClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmiAL_ClearCompleted: TMenuItem
      Caption = 'Clear completed items'
      OnClick = pmiAL_ClearCompletedClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object pmiAL_Tools: TMenuItem
      Caption = 'Tools'
      Enabled = False
      object TMenuItem
      end
    end
  end
  object diaOpenArchive: TOpenDialog
    Filter = 
      'All supported files (*.scs; *.zip)|*.scs;*.zip|ZIP archive (*.zi' +
      'p)|*.zip|SCS mod archive (*.scs)|*.scs|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 728
  end
  object ilArchiveList: TImageList
    Left = 696
  end
  object tmrAnimTimer: TTimer
    OnTimer = tmrAnimTimerTimer
    Left = 664
  end
end
