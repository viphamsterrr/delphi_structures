unit Sort;

interface

uses System.Rtti, System.TypInfo, StraDat;

type Insertion<T: IComparable<T>> = class
public
  procedure sort(var a: TArray<T>; order: boolean = false);

private
  procedure ascending (var a: TArray<T>);
  procedure descending (var a: TArray<T>);
end;

type Service<T>  = class
public
   procedure exchange(var a: TArray<T>; first: integer; second: integer);
end;

type Shell<T: IComparable<T>> = class
public
  procedure sort(var a: TArray<T>; order: boolean = false);

private
  procedure ascending (var a: TArray<T>);
  procedure descending (var a: TArray<T>);
  function indexes (size: integer): Tarray<integer>;
  function powint(base: integer; power: integer): integer;
end;

implementation

procedure Insertion<T>.sort(var a: TArray<T>; order: boolean = false);
begin
    if order
    then
      descending(a)
    else
      ascending(a)
end;

procedure Insertion<T>.ascending (var a: TArray<T>);
var i, j: integer;
    exch: Service<T>;
begin
  exch:= Service<T>.Create;
  for i:= 0 to length(a) - 2
  do
  begin
    j:= i + 1;
    while a[j].CompareTo(a[j - 1]) < 0
    do
    begin
      exch.exchange(a, j, j - 1);
      j:= j - 1;
      if j = 0 then Break
    end
  end
end;

procedure Insertion<T>.descending (var a: TArray<T>);
var i, j: integer;
    exch: Service<T>;
begin
  exch:= Service<T>.Create;
  for i:= 0 to length(a) - 2
  do
  begin
    j:= i + 1;
    while a[j].CompareTo(a[j - 1]) > 0
    do
    begin
      exch.exchange(a, j, j - 1);
      j:= j - 1;
      if j = 0 then Break
    end
  end
end;

procedure Service<T>.exchange(var a: TArray<T>; first: integer; second: integer);
var intmd: T;
begin
  intmd:= a[first];
  a[first]:= a[second];
  a[second]:= intmd
end;

procedure Shell<T>.sort(var a: TArray<T>; order: boolean = false);
begin
    if order
    then
      descending(a)
    else
      ascending(a)
end;

procedure Shell<T>.ascending (var a: TArray<T>);
var i, j, idx: integer;
    idxs: TArray<integer>;
    exch: Service<T>;
begin
  exch:= Service<T>.Create;
  idxs:= indexes(Length(a));
  for idx in idxs
  do
  begin
    i:= idx;
    while i < Length(a)
    do
    begin
      j:= i;
      while j >= idx
      do
      begin
        if a[j].compareTo(a[j - idx]) < 0
        then
          exch.exchange(a, j, j - idx);
          j:= j - idx
      end;
      i:= i + 1
    end
  end
end;

procedure Shell<T>.descending (var a: TArray<T>);
var i, j, idx: integer;
    idxs: TArray<integer>;
    exch: Service<T>;
begin
  exch:= Service<T>.Create;
  idxs:= indexes(Length(a));
  for idx in idxs
  do
  begin
    i:= idx;
    while i < Length(a)
    do
    begin
      j:= i;
      while j >= idx
      do
      begin
        if a[j].compareTo(a[j - idx]) > 0
        then
          exch.exchange(a, j, j - idx);
          j:= j - idx
      end;
      i:= i + 1
    end
  end
end;

function Shell<T>.indexes(size: Integer): Tarray<integer>;
var first, second: Stack<Integer>;
    third: Queue<integer>;
    i, res: integer;
begin
  first:= Stack<Integer>.Create;
  i:= 0;
  while true
  do
  begin
    res:= powint(4, i) * 9 - powint(2, i) * 9 + 1;
    if res <= size div 2
    then
    begin
      if res > 0 then first.put(res)
    end
    else break;
    i:= i + 1
  end;
  second:= Stack<integer>.Create;
  i:= 0;
  while true
  do
  begin
    res:= powint(4, i) - powint(2, i) * 3 + 1;
    if res <= size div 2 then if res > 0 then second.put(res)
    else break;
    i:= i + 1
  end;
  third:= Queue<integer>.Create;
  while not first.isEmpty or not second.isEmpty
  do
    if not first.isEmpty and not second.isEmpty and (first.Pick = second.Pick) then first.take
    else
      if (not first.isEmpty and second.isEmpty) or (first.Pick > second.Pick) then third.put(first.take)
      else
        third.put(second.take);
  i:= 0;
  SetLength(result, third.getSize);
  while not third.isEmpty
  do
  begin
    result[i]:= third.take;
    i:= i + 1
  end
end;

function Shell<T>.powint(base: integer; power: integer): integer;
begin
  if power = 1 then result:= base
  else
    if power  = 0 then result:= 1
    else
      if power mod 2 = 0 then result:= powint(base, power div 2) * powint(base, power div 2)
      else
        result:= powint(base, power - 1) * base
end;

end.
