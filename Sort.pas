unit Sort;

interface

uses StraDat;

type Insertion<T: IComparable<T>> = class
public
  procedure sort(var a: TArray<T>; order: boolean = false);

private
  procedure ascending (var a: TArray<T>);
  procedure descending (var a: TArray<T>);
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

type Merge<T: IComparable<T>> = class
public
  procedure sort(var a: TArray<T>; order: boolean = false);

private
  procedure ascending (var a: TArray<T>); overload;
  procedure ascending (var a: TArray<T>; var b: TArray<T>; start, finish: integer); overload;
  procedure descending (var a: TArray<T>); overload;
  procedure descending (var a: TArray<T>; var b: TArray<T>; start, finish: integer); overload;
  procedure mergeAsc (var a: TArray<T>; var b: TArray<T>; start, middle, finish: integer);
  procedure mergeDesc (var a: TArray<T>; var b: TArray<T>; start, middle, finish: integer);
  function isSortedAsc (a: TArray<T>; start, finish: integer): boolean;
  function isSortedDesc (a: TArray<T>; start, finish: integer): boolean;
end;

type Service<T>  = class
public
   procedure exchange(var a: TArray<T>; first: integer; second: integer);
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

procedure Merge<T>.sort(var a: TArray<T>; order: boolean = false);
begin
  if order then descending(a) else ascending(a)
end;

procedure Merge<T>.ascending (var a: TArray<T>);
var b: TArray<T>;
begin
  SetLength(b, Length(a));
  ascending(a, b, 0, length(a));
end;

procedure Merge<T>.ascending (var a: TArray<T>; var b: TArray<T>; start, finish: integer);
var middle: integer;
begin
  middle:= start + (finish - start) div 2;
  if not isSortedAsc(a, start, middle) then ascending(a, b, start, middle);
  if not isSortedAsc(a, middle, finish) then ascending(a, b, middle, finish);
  mergeAsc(a, b, start, middle, finish)
end;

procedure Merge<T>.descending (var a: TArray<T>);
var b: TArray<T>;
begin
  SetLength(b, Length(a));
  descending(a, b, 0, length(a));
end;

procedure Merge<T>.descending (var a: TArray<T>; var b: TArray<T>; start, finish: integer);
var middle: integer;
begin
  middle:= start + (finish - start) div 2;
  if not isSortedDesc(a, start, middle) then descending(a, b, start, middle);
  if not isSortedDesc(a, middle, finish) then descending(a, b, middle, finish);
  mergeDesc(a, b, start, middle, finish)
end;

procedure Merge<T>.mergeAsc (var a: TArray<T>; var b: TArray<T>; start, middle, finish: integer);
var i, j, k: integer;
begin
  for i:= start to finish - 1
  do
    b[i]:= a[i];
  i:= start;
  j:= start;
  k:= middle;
  while (j < middle) or (k < finish)
  do
  begin
    if j = middle
    then
    begin
      a[i]:= b[k];
      inc(i);
      inc(k)
    end
    else
    begin
      if k = finish
      then
      begin
        a[i]:= b[j];
        inc(i);
        inc(j)
      end
      else
      begin
        if b[k].CompareTo(b[j]) > 0
        then
        begin
          a[i]:= b[j];
          inc(i);
          inc(j)
        end
        else
        begin
          a[i]:= b[k];
          inc(i);
          inc(k)
        end
      end
    end
  end
end;

procedure Merge<T>.mergeDesc (var a: TArray<T>; var b: TArray<T>; start, middle, finish: integer);
var i, j, k: integer;
begin
  for i:= start to finish - 1
  do
    b[i]:= a[i];
  i:= start;
  j:= start;
  k:= middle;
  while (j < middle) or (k < finish)
  do
  begin
    if j = middle
    then
    begin
      a[i]:= b[k];
      inc(i);
      inc(k)
    end
    else
    begin
      if k = finish
      then
      begin
        a[i]:= b[j];
        inc(i);
        inc(j)
      end
      else
      begin
        if b[k].CompareTo(b[j]) < 0
        then
        begin
          a[i]:= b[j];
          inc(i);
          inc(j)
        end
        else
        begin
          a[i]:= b[k];
          inc(i);
          inc(k)
        end
      end
    end
  end
end;

function Merge<T>.isSortedAsc (a: TArray<T>; start, finish: integer): boolean;
var i: integer;
begin
  result:= true;
  if finish - start > 1
  then
    for i:= start to finish - 2
    do
      if a[i + 1].CompareTo(a[i]) < 0
      then
      begin
        result:= false;
        break
      end
end;

function Merge<T>.isSortedDesc (a: TArray<T>; start, finish: integer): boolean;
var i: integer;
begin
  result:= true;
  if finish - start > 1
  then
    for i:= start to finish - 2
    do
      if a[i + 1].CompareTo(a[i]) > 0
      then
      begin
        result:= false;
        break
      end
end;

end.
