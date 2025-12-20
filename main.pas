unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, StraDat, Generics.Collections;

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

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Go1ButtonClick(Sender: TObject);
var s: RandStack<int64>;
    i, j: int64;
begin
  s:= RandStack<Int64>.Create(7);
  s.put(9);
  s.put(-6);
  s.put(22);
  s.put(-5);
  for i:= 17 to 199
  do
    if i mod 4 = 0
    then
      s.put(i);
//  for i in s do ShowMessage(i.ToString);
  while not s.isEmpty
  do
    showmessage(s.take.ToString)
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
