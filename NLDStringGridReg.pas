unit NLDStringGridReg;

interface

uses
  Classes, NLDStringGrid, NLDStringGridEdit, DesignIntf;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NLDelphi', [TNLDStringGrid]);
  RegisterComponentEditor(TNLDStringGrid, TNLDStringGridEditor);
end;

end.
