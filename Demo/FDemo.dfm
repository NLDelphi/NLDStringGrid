object DemoForm: TDemoForm
  Left = 347
  Top = 309
  Width = 953
  Height = 727
  Caption = 'DemoForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TNLDStringGrid
    Left = 0
    Top = 129
    Width = 945
    Height = 332
    Columns.OnChanged = GridColumnsChanged
    Columns = <
      item
        RowNumbers = True
        Title.Caption = ' '
      end
      item
        EditFormat = '%s_543'
        InputStyle = isFloat
        Title.Caption = 'Column1'
      end
      item
        Action = Action1
        MultiLine = True
      end
      item
        EditStyle = esEllipsis
        Title.Caption = 'Column3'
      end
      item
        MaxLength = 10
        Title.Caption = 'Column4'
      end
      item
        EditStyle = esPickListOnly
        MaxWidth = 100
        PickListItems.Strings = (
          'Delphi'
          'IDE'
          'Pascal'
          'Procedure'
          'Function')
        Title.Caption = 'Column5'
      end>
    ColCount = 12
    OnTitleClick = GridTitleClick
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goRowMoving, goColMoving, goTabs, goThumbTracking]
    ReadOnlyColor = 13290239
    StretchModes = [smAllowStretchRight, smAllowStretchAll, smAllowShrinkRight, smAllowShrinkAll]
    Align = alClient
    AutoRowHeights = True
    DefaultColWidth = 76
    DefaultRowHeight = 26
    OnEditButtonClick = GridEditButtonClick
    RowCount = 15
    TabOrder = 0
    ColWidths = (
      77
      77
      78
      78
      78
      78
      77
      77
      77
      77
      77
      77)
    RowHeights = (
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18)
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 461
    Width = 945
    Height = 218
    Align = alBottom
    TabOrder = 1
    object GroupBox2: TGroupBox
      Left = 8
      Top = 8
      Width = 177
      Height = 201
      Caption = 'Selection operations:'
      TabOrder = 0
      object Button5: TButton
        Left = 8
        Top = 16
        Width = 161
        Height = 25
        Caption = 'Toggle ReadOnly'
        TabOrder = 0
        OnClick = Button5Click
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 48
        Width = 161
        Height = 145
        Caption = 'Merge cells:'
        TabOrder = 1
        object Button7: TButton
          Left = 8
          Top = 16
          Width = 145
          Height = 25
          Caption = 'Only background'
          TabOrder = 0
          OnClick = Button7Click
        end
        object Button8: TButton
          Left = 8
          Top = 48
          Width = 145
          Height = 25
          Caption = 'Background + text'
          TabOrder = 1
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 8
          Top = 80
          Width = 145
          Height = 25
          Caption = 'Background + text multiline'
          TabOrder = 2
          OnClick = Button9Click
        end
        object Button10: TButton
          Left = 8
          Top = 112
          Width = 145
          Height = 25
          Caption = 'Cancel'
          TabOrder = 3
          OnClick = Button10Click
        end
      end
    end
    object GroupBox3: TGroupBox
      Left = 208
      Top = 8
      Width = 297
      Height = 201
      Caption = 'Grid operations:'
      TabOrder = 1
      object Button1: TButton
        Left = 8
        Top = 16
        Width = 97
        Height = 25
        Caption = 'Reset all colors'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 8
        Top = 48
        Width = 97
        Height = 25
        Caption = 'Reset all fonts'
        TabOrder = 1
        OnClick = Button2Click
      end
      object GroupBox4: TGroupBox
        Left = 8
        Top = 80
        Width = 209
        Height = 113
        Caption = 'Gradient Columncolors:'
        TabOrder = 2
        object SpeedButton1: TSpeedButton
          Left = 8
          Top = 80
          Width = 97
          Height = 25
          AllowAllUp = True
          GroupIndex = 1
          Down = True
          Caption = 'Columns only'
        end
        object Label2: TLabel
          Left = 8
          Top = 20
          Width = 22
          Height = 13
          Caption = 'First:'
        end
        object Label3: TLabel
          Left = 8
          Top = 52
          Width = 23
          Height = 13
          Caption = 'Last:'
        end
        object ColorBox1: TColorBox
          Left = 40
          Top = 16
          Width = 161
          Height = 22
          DefaultColorColor = clWindow
          Selected = clWindow
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
        object ColorBox2: TColorBox
          Left = 40
          Top = 48
          Width = 161
          Height = 22
          DefaultColorColor = clWindow
          Selected = clWindow
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 1
        end
        object Button4: TButton
          Left = 112
          Top = 80
          Width = 89
          Height = 25
          Caption = 'Set'
          TabOrder = 2
          OnClick = Button4Click
        end
      end
      object Button3: TButton
        Left = 112
        Top = 16
        Width = 81
        Height = 25
        Caption = 'Import CSV'
        TabOrder = 3
        OnClick = Button3Click
      end
      object Button6: TButton
        Left = 112
        Top = 48
        Width = 81
        Height = 25
        Caption = 'Export CSV'
        TabOrder = 4
        OnClick = Button6Click
      end
      object Button11: TButton
        Left = 200
        Top = 16
        Width = 89
        Height = 25
        Caption = 'Add Column'
        TabOrder = 5
        OnClick = Button11Click
      end
    end
    object GroupBox5: TGroupBox
      Left = 528
      Top = 8
      Width = 409
      Height = 201
      Caption = 'Grid properties:'
      TabOrder = 2
      object SpeedButton2: TSpeedButton
        Left = 8
        Top = 16
        Width = 193
        Height = 25
        AllowAllUp = True
        GroupIndex = 2
        Caption = 'SyncColumns'
        OnClick = SpeedButton2Click
      end
      object Label1: TLabel
        Left = 208
        Top = 20
        Width = 70
        Height = 13
        Caption = 'GridLineWidth:'
      end
      object SpeedButton3: TSpeedButton
        Left = 8
        Top = 48
        Width = 193
        Height = 25
        AllowAllUp = True
        GroupIndex = 2
        Caption = 'Editing'
        OnClick = SpeedButton3Click
      end
      object Label4: TLabel
        Left = 8
        Top = 171
        Width = 79
        Height = 13
        Caption = 'FocusRectColor:'
      end
      object Label5: TLabel
        Left = 208
        Top = 51
        Width = 71
        Height = 13
        Caption = 'SelectionColor:'
      end
      object SpinEdit1: TSpinEdit
        Left = 288
        Top = 16
        Width = 113
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 1
        OnChange = SpinEdit1Change
      end
      object RadioGroup1: TRadioGroup
        Left = 8
        Top = 80
        Width = 193
        Height = 81
        Caption = 'FocusRectStyle'
        ItemIndex = 0
        Items.Strings = (
          'frDefault'
          'frSolidAutoBW'
          'frSolidCustomColor')
        TabOrder = 1
        OnClick = RadioGroup1Click
      end
      object ColorBox3: TColorBox
        Left = 96
        Top = 168
        Width = 105
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 2
        OnChange = ColorBox3Change
      end
      object ColorBox4: TColorBox
        Left = 288
        Top = 48
        Width = 113
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 3
        OnChange = ColorBox4Change
      end
      object GroupBox6: TGroupBox
        Left = 208
        Top = 80
        Width = 193
        Height = 113
        Caption = 'AlternatingRowColors:'
        TabOrder = 4
        object Label6: TLabel
          Left = 8
          Top = 20
          Width = 28
          Height = 13
          Caption = 'Even:'
        end
        object Label7: TLabel
          Left = 8
          Top = 50
          Width = 23
          Height = 13
          Caption = 'Odd:'
        end
        object ColorBox5: TColorBox
          Left = 40
          Top = 16
          Width = 145
          Height = 22
          DefaultColorColor = clWindow
          Selected = clWindow
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
          OnChange = ColorBox5Change
        end
        object ColorBox6: TColorBox
          Left = 40
          Top = 48
          Width = 145
          Height = 22
          DefaultColorColor = clWindow
          Selected = clWindow
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 1
          OnChange = ColorBox6Change
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 80
          Width = 89
          Height = 17
          Caption = 'IncludeFixed'
          TabOrder = 2
          OnClick = CheckBox1Click
        end
        object CheckBox2: TCheckBox
          Left = 104
          Top = 72
          Width = 81
          Height = 33
          Caption = 'Override ColumnColor'
          TabOrder = 3
          WordWrap = True
          OnClick = CheckBox2Click
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 679
    Width = 945
    Height = 19
    AutoHint = True
    Panels = <>
    SimplePanel = True
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 945
    Height = 129
    Align = alTop
    TabOrder = 3
    object GroupBox7: TGroupBox
      Left = 8
      Top = 8
      Width = 929
      Height = 113
      Caption = 'Testing features:'
      TabOrder = 0
      object Memo1: TMemo
        Left = 8
        Top = 16
        Width = 257
        Height = 89
        Lines.Strings = (
          ''
          'Old MacDonald had a farm.'
          'Hia, hia, ho!'
          'And on that farm there were some chickens.'
          'Hia, hia, ho!'
          'And the chickens went outside to cows.'
          'Hia, hia, ho!'
          'And asked why they were small to them.'
          'Hia, hia, ho!'
          'The cows replied they might be ill.'
          'Hia, hia, ho!'
          'Isn'#39't that the stress today?'
          'Hia, hia, ho!')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Button12: TButton
        Left = 272
        Top = 16
        Width = 153
        Height = 25
        Caption = 'Copy memo to Cols[3]'
        TabOrder = 1
        OnClick = Button12Click
      end
      object Button13: TButton
        Left = 272
        Top = 80
        Width = 153
        Height = 25
        Caption = 'Exchange Columns 3 and 4'
        TabOrder = 2
        OnClick = Button13Click
      end
      object Button14: TButton
        Left = 272
        Top = 48
        Width = 153
        Height = 25
        Caption = 'AutoWidth Columns[3]'
        Enabled = False
        TabOrder = 3
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 432
        Top = 16
        Width = 209
        Height = 25
        Caption = 'Runtime exception handling example'
        TabOrder = 4
        OnClick = Button15Click
      end
      object Button16: TButton
        Left = 432
        Top = 48
        Width = 209
        Height = 25
        Caption = 'Raise all possible custom exceptions'
        TabOrder = 5
        OnClick = Button16Click
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 4000
    OnTimer = Timer1Timer
    Left = 896
    Top = 24
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 856
    Top = 72
    object Action1: TAction
      Caption = 'Action1'
      ImageIndex = 0
      OnExecute = Action1Execute
    end
  end
  object ImageList1: TImageList
    Left = 834
    Top = 36
    Bitmap = {
      494C010101000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F3F3F300DBDBDB00DBDBDB00F3F3F30000000000F3F3F300DBDBDB00DBDB
      DB00F3F3F3000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F3F3
      F300C3C3C3008787870087878700B7B7B700CFCFCF00B7B7B700878787008787
      8700C3C3C300F3F3F30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DBDB
      DB0031470200314702004B4B4B00636363006F6F6F0031470200314702004B4B
      4B0087878700DBDBDB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F3F3F3003147
      02002E8A3C002E8A3C003147020031470200314702002E8A3C002E8A3C003147
      020063636300B7B7B700F3F3F300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F3F3F300C3C3C3003147
      02002E8A3C002E8A3C002E8A3C002E8A3C002E8A3C002E8A3C002E8A3C003147
      02004B4B4B007B7B7B00C3C3C300F3F3F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DBDBDB00314702002E8A
      3C002E8A3C002E8A3C002E8A3C002E8A3C002E8A3C002E8A3C002E8A3C002E8A
      3C00314702004B4B4B0087878700DBDBDB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F3F3F300314702002E8A3C002E8A
      3C002E8A3C00314702002E8A3C002E8A3C002E8A3C00314702002E8A3C002E8A
      3C002E8A3C003147020063636300B7B7B7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDB00314702002E8A3C002E8A
      3C003147020087878700314702002E8A3C0031470200C3C3C300314702002E8A
      3C002E8A3C00314702004B4B4B00878787000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000314702002E8A3C002E8A3C002E8A
      3C0031470200C3C3C300F3F3F30031470200E7E7E700F3F3F300314702002E8A
      3C002E8A3C002E8A3C00314702006F6F6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000314702002E8A3C002E8A3C003147
      0200C3C3C300F3F3F30000000000000000000000000000000000000000003147
      02002E8A3C002E8A3C0031470200939393000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000314702003147020031470200DBDB
      DB00F3F3F3000000000000000000000000000000000000000000000000000000
      0000314702003147020031470200DBDBDB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      F087000000000000E003000000000000E003000000000000C001000000000000
      8000000000000000800000000000000000000000000000000000000000000000
      000000000000000003E000000000000007F0000000000000FFFF000000000000
      FFFF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
end
