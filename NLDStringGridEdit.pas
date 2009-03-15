unit NLDStringGridEdit;

interface

uses
  DesignEditors, NLDStringGrid, ColnEdit;

type
  TNLDStringGridEditor = class(TComponentEditor)
  private
    function StringGrid: TNLDStringGrid;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{ TNLDStringGridEditor }

const
  SEditorVerb0 = 'Columns Editor...';
  SColumnsPropName = 'Columns';

procedure TNLDStringGridEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowCollectionEditor(Designer, Component, StringGrid.Columns,
        SColumnsPropName);
  end;
end;

function TNLDStringGridEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0:
      Result := SEditorVerb0;
    else
      Result := '';
  end;
end;

function TNLDStringGridEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TNLDStringGridEditor.StringGrid: TNLDStringGrid;
begin
  Result := TNLDStringGrid(Component);
end;

end.
