unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, StraDat, Sort, Generics.Collections,
  Generics.Defaults;

type
  TForm1 = class(TForm)
    Value1Edit: TEdit;
    Value2Edit: TEdit;
    Value3Edit: TEdit;
    GoButton: TButton;
    Go1Button: TButton;
    procedure GoButtonClick(Sender: TObject);
    procedure Go1ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  IntCom = Class (TInterfacedObject, IComparable, IComparable<IntCom>)

  public
    value: integer;
  public
    function CompareTo(obj: TObject): integer; overload;
    function CompareTo(second: IntCom): integer; overload;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

function IntCom.CompareTo(obj: TObject): integer;
var second: IntCom;
begin
  second:= obj as IntCom;
  if self.value > second.value then result:= 1
  else if self.value < second.value then result:= - 1
       else result:= 0
end;

function IntCom.CompareTo(second: IntCom): integer;
begin
  if self.value > second.value then result:= 1
  else if self.value < second.value then result:= - 1
       else result:= 0
end;

procedure TForm1.Go1ButtonClick(Sender: TObject);
var s: TArray<IntCom>;
    i: int64;
    rf, rs: string;
    sorting: Merge<IntCom>;
    d: IntCom;
begin
  SetLength(s, 30);
  for i:= 0 to 29
  do
  begin
    s[i]:= IntCom.Create;
    s[i].value:= Random(99);
  end;
  rf:= '';
  rs:= '';
  for i:= 0 to 29
  do
    rf:= rf + s[i].value.ToString + ' ';
  sorting:= Merge<IntCom>.Create;
  sorting.sort(s);
  rs:= '';
  for d in s
  do
    rs:= rs + d.value.ToString + ' ';
  ShowMessage(rf);
  ShowMessage(rs)
end;

procedure TForm1.GoButtonClick(Sender: TObject);
var s, t: AStack<Int64>;
    m: TList<AStack<int64>>;
    it, sm: int64;
begin
  m:= TList<AStack<int64>>.Create;
  s:= AStack<Int64>.Create;
  t:= AStack<Int64>.Create;
  s.put(1);
  s.put(-1);
  s.put(-1);
  s.put(-1);
  s.put(-1);
  s.put(-1);
  t.put(1);
  t.put(1);
  t.put(1);
  t.put(1);
  m.Add(s);
  m.Add(t);
  m.Sort;
  showmessage(m[0].getSize.ToString)
end;

end.
