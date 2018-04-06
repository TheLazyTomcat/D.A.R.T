object fResultInfoForm: TfResultInfoForm
  Left = 551
  Top = 391
  BorderStyle = bsDialog
  Caption = 'Result information'
  ClientHeight = 336
  ClientWidth = 712
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object lblProcessingResult: TLabel
    Left = 8
    Top = 8
    Width = 110
    Height = 13
    Caption = 'lblProcessingResult'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblCopyHint: TLabel
    Left = 411
    Top = 320
    Width = 294
    Height = 13
    Alignment = taRightJustify
    Caption = 'Press <Ctrl + C> to copy displayed information into clipboard'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object meResultInfo: TMemo
    Left = 8
    Top = 32
    Width = 697
    Height = 281
    TabStop = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyPress = meResultInfoKeyPress
  end
end
