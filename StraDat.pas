unit StraDat;

interface

uses SysUtils;

// Stack
// Comparable. Comaparing by their sizes
// Iterable.

type Stack<T> = class(TInterfacedObject, IComparable)

  private
    type PNodeSmall = ^NodeSmall;
      NodeSmall = record
      next: ^NodeSmall;
      item: T;
    end;

  private
  type Enumerator = class
    private cur, first: PNodeSmall;
            idx: integer;
            already: boolean;
    public
           constructor Create(stack: Stack<T>);
           function GetCurrent: T;
           function MoveNext: boolean;
           procedure Reset;
           property Current: T read GetCurrent;
  end;

  private
    first: PNodeSmall;
    size: integer;

  public
    function getSize: integer;
    procedure put(const thing: T);
    function take: T;
    function isEmpty: boolean;
    function Contains(thing: T): boolean;
    function Pick: T;
    constructor Create; overload;
    constructor Create(thing: T); overload;
    destructor Destroy; override;
    function GetEnumerator: Enumerator;
    function CompareTo(obj: TObject): integer;
end;

// Queue
// Comparable. Comaparing by their sizes
// Iterable. Available to 'for..in' pattern
// FIFO queue

type Queue<T> = class(TInterfacedObject, IComparable)

private
  type PNodeSmall = ^NodeSmall;
    NodeSmall = record
    next: ^NodeSmall;
    item: T;
  end;

  private
  type Enumerator = class
    private cur, first: PNodeSmall;
            idx: integer;
            already: boolean;
    public
           constructor Create(queue: Queue<T>);
           function GetCurrent: T;
           function MoveNext: boolean;
           procedure Reset;
           property Current: T read GetCurrent;
  end;

  private
    first, last: PNodeSmall;
    size: integer;

  public
    function getSize: integer;
    procedure put(const thing: T);
    function take: T;
    function isEmpty: boolean;
    function Contains(thing: T): boolean;
    function PickFirst: T;
    function PickLast: T;
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: Enumerator;
    function CompareTo(obj: TObject): integer;
end;

// Double-edged queue
// Comparable. Comaparing by their sizes
// Iterable. Available to 'for..in' pattern
// Queue with adding and removing from both sides of structure

type DEQueue<T> = class(TInterfacedObject, IComparable)

  private
    type PNodeDE = ^NodeDE;
      NodeDE = record
      prev, next: ^NodeDE;
      item: T;
    end;

  private
  type Enumerator = class
    private cur, first: PNodeDE;
            idx: integer;
            already: boolean;
    public
           constructor Create(dequeue: DEQueue<T>);
           function GetCurrent: T;
           function MoveNext: boolean;
           procedure Reset;
           property Current: T read GetCurrent;
  end;

  private
    first, last: PNodeDE;
    size: integer;

  public
    function getSize: integer;
    procedure putFirst(const thing: T);
    procedure putLast(const thing: T);
    function takeFirst: T;
    function takeLast: T;
    function isEmpty: boolean;
    function Contains(thing: T): boolean;
    function PickFirst: T;
    function PickLast: T;
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: Enumerator;
    function CompareTo(obj: TObject): integer;
end;

// Randomized stack
// Comparable. Comaparing by their sizes
// Iterable. Available to 'for..in' pattern
// Stack with randomized pickuping and taking elements

type RandStack<T> = class(TInterfacedObject, IComparable)

  private things: array of T;
          size: integer;
          procedure Refactor(newsize: integer);

  private
    type Enumerator = class
      private idx: integer;
              perm: array of T;
      public
              constructor Create(randst: RandStack<T>);
              function GetCurrent: T;
              function MoveNext: boolean;
              procedure Reset;
              property Current: T read GetCurrent;
    end;

  public
    function getSize: integer;
    procedure put(const thing: T);
    function take: T;
    function isEmpty: boolean;
    function Contains(thing: T): boolean;
    function pick: T;
    constructor Create; overload;
    constructor Create(thing: T); overload;
    destructor Destroy; override;
    function GetEnumerator: Enumerator;
    function CompareTo(obj: TObject): integer;
end;

// Stack with array imlementation
// Comparable. Comaparing by their sizes
// Iterable. Available to 'for..in' pattern
// Stack with array (instead of link list) implementation

type AStack<T> = class(TInterfacedObject, IComparable)

  private things: array of T;
          size: integer;
          procedure Refactor(newsize: integer);

  private
    type Enumerator = class
      private idx: integer;
              iter: array of T;
      public
              constructor Create(ast: AStack<T>);
              function GetCurrent: T;
              function MoveNext: boolean;
              procedure Reset;
              property Current: T read GetCurrent;
    end;

  public
    function getSize: integer;
    procedure put(const thing: T);
    function take: T;
    function isEmpty: boolean;
    function Contains(thing: T): boolean;
    function pick: T;
    function pickRand: T;
    constructor Create; overload;
    constructor Create(thing: T); overload;
    destructor Destroy; override;
    function GetEnumerator: Enumerator;
    function CompareTo(obj: TObject): integer;
end;

  implementation

//create an empty stack
constructor Stack<T>.Create;
begin
  inherited;
  first:= new(PNodeSmall);
  first.next:= nil;
  size:= 0
end;

//create a empty stack with initial elemement
constructor Stack<T>.Create(thing: T);
begin
  first:= new(PNodeSmall);
  first.next:= nil;
  size:= 1;
  first.item:= thing
end;

//returns amount of elements in stack
function Stack<T>.getSize: integer;
begin
  result:= size
end;

//add an element to stack
procedure Stack<T>.put(const thing: T);
var intmd: PNodeSmall;
begin
  if size = 0
  then
  begin
    first.item:= thing;
    size:= 1
  end
  else
  begin
    intmd:= new (PNodeSmall);
    intmd^.item:= thing;
    intmd^.next:= first;
    first:= intmd;
    inc(size)
  end
end;

//return first element in stack and deletes it there
function Stack<T>.take: T;
begin
  if size = 0
  then
    raise Exception.Create('You attempted to take a value from stack but its empty.');
  if size = 1
  then
  begin
    result:= first.item;
    size:= 0
  end
  else
  begin
    result:= first.item;
    first:= first.next;
    dec(size)
  end;
end;

//checks against presence of any element in stack
function Stack<T>.isEmpty: boolean;
begin
  result:= (size = 0);
end;

//checks presence of given element in stack
function Stack<T>.Contains(thing: T): boolean;
var current: PNodeSmall;
    szchk: integer;
begin
  result:= false;
  current:= first;
  szchk:= size;
  while not (szchk = 0)
  do
  begin
    if current.item = thing
    then
    begin
      result:= true;
      break
    end
    else
    begin;
      current:= current.next;
      dec(szchk)
    end;
  end
end;

//returns first element in stack without deleting
function Stack<T>.Pick: T;
begin
  if isEmpty
  then
    raise Exception.Create('You attempted to pick a value from stack but its empty.')
  else
    result:= first.item
end;

// Comparator
function Stack<T>.CompareTo(obj: TObject): integer;
begin
  if self.ClassType <> obj.ClassType
  then
    raise EArgumentException.Create('Illegal argument in array elements');
  if self.size > (obj as Stack<T>).size
  then
      result:= 1
  else
    if self.size < (obj as Stack<T>).size
    then
      result:= -1
    else
      result:= 0
end;

//deletes the stack
destructor Stack<T>.Destroy;
begin
  while not isEmpty
  do
    take;
  inherited
end;

//create an empty queue
constructor Queue<T>.Create;
begin
  inherited;
  first:= new(PNodeSmall);
  first.next:= nil;
  last:= first;
  size:= 0
end;

//returns amount of elements in queue
function Queue<T>.getSize: integer;
begin
  result:= size
end;

//add an element to the beginning of queue
procedure Queue<T>.put(const thing: T);
var intmd: PNodeSmall;
begin
  if size = 0
  then
  begin
    first.item:= thing;
    size:= 1
  end
  else
  begin
    intmd:= new (PNodeSmall);
    intmd^.item:= thing;
    intmd^.next:= nil;
    first.next:= intmd;
    first:= intmd;
    inc(size)
  end
end;

//return first element at the end of queue and deletes it there
function Queue<T>.take: T;
begin
  if size = 0
  then
    raise Exception.Create('You attempted to take a value from queue but its empty.');
  if size = 1
  then
  begin
    result:= first.item;
    size:= 0
  end
  else
  begin
    result:= last.item;
    last:= last.next;
    dec(size)
  end;
end;

//checks against presence of any element in queue
function Queue<T>.isEmpty: boolean;
begin
  result:= (size = 0);
end;

//checks presence of given element in queue
function Queue<T>.Contains(thing: T): boolean;
var current: PNodeSmall;
    szchk: integer;
begin
  result:= false;
  current:= last;
  szchk:= size;
  while not (szchk = 0)
  do
  begin
    if current.item = thing
    then
    begin
      result:= true;
      break
    end
    else
    begin;
      current:= current.next;
      dec(szchk)
    end;
  end
end;

//returns the first element in queue without deleting
function Queue<T>.PickFirst: T;
begin
  if isEmpty
  then
    raise Exception.Create('You attempted to pick a value from queue but its empty.')
  else
    result:= last.item
end;

//returns the last element in queue without deleting
function Queue<T>.PickLast: T;
begin
  if isEmpty
  then
    raise Exception.Create('You attempted to pick a value from queue but its empty.')
  else
    result:= first.item
end;

// Comparator
function Queue<T>.CompareTo(obj: TObject): integer;
begin
  if self.ClassType <> obj.ClassType
  then
    raise EArgumentException.Create('Illegal argument in array elements');
  if self.size > (obj as Queue<T>).size
  then
      result:= 1
  else
    if self.size < (obj as Queue<T>).size
    then
      result:= -1
    else
      result:= 0
end;

//deletes the queue
destructor Queue<T>.Destroy;
begin
  while not isEmpty
  do
    take;
  inherited
end;

//create an empty double-edged queue
constructor DEQueue<T>.Create;
begin
  inherited;
  first:= new(PNodeDE);
  first.next:= nil;
  first.prev:= nil;
  last:= first;
  size:= 0
end;

//returns amount of elements in double-edged queue
function DEQueue<T>.getSize: Integer;
begin
  result:= size
end;

//add an element to the beginning of double-edged queue
procedure DEQueue<T>.putFirst(const thing: T);
var intmd: PNodeDE;
begin
  if size = 0
  then
  begin
    first.item:= thing;
    size:= 1
  end
  else
  begin
    intmd:= new (PNodeDE);
    intmd^.item:= thing;
    intmd^.prev:= nil;
    intmd^.next:= first;
    first.prev:= intmd;
    first:= intmd;
    inc(size)
  end
end;

//add an element to the beginning of double-edged queue
procedure DEQueue<T>.putLast(const thing: T);
var intmd: PNodeDE;
begin
  if size = 0
  then
  begin
    first.item:= thing;
    size:= 1
  end
  else
  begin
    intmd:= new (PNodeDE);
    intmd^.item:= thing;
    intmd^.next:= nil;
    intmd^.prev:= last;
    last.next:= intmd;
    last:= intmd;
    inc(size)
  end
end;

//return the first element of double-edged queue and deletes it there
function DEQueue<T>.takeFirst: T;
begin
  if size = 0
  then
    raise Exception.Create('You attempted to take a value from queue but its empty.');
  if size = 1
  then
  begin
    result:= first.item;
    size:= 0
  end
  else
  begin
    result:= first.item;
    first:= first.next;
    dec(size)
  end;
end;

//return the last element of double-edged queue and deletes it there
function DEQueue<T>.takeLast: T;
begin
  if size = 0
  then
    raise Exception.Create('You attempted to take a value from queue but its empty.');
  if size = 1
  then
  begin
    result:= last.item;
    size:= 0
  end
  else
  begin
    result:= last.item;
    last:= last.prev;
    dec(size)
  end;
end;

//checks against presence of any element in double-edged queue
function DEQueue<T>.isEmpty: boolean;
begin
  result:= (size = 0);
end;

//checks presence of given element in double-edged queue
function DEQueue<T>.Contains(thing: T): boolean;
var current: PNodeDE;
    szchk: integer;
begin
  result:= false;
  current:= first;
  szchk:= size;
  while not (szchk = 0)
  do
  begin
    if current.item = thing
    then
    begin
      result:= true;
      break
    end
    else
    begin;
      current:= current.next;
      dec(szchk)
    end;
  end
end;

//returns the first element in double-edged queue without deleting
function DEQueue<T>.PickFirst: T;
begin
  if isEmpty
  then
    raise Exception.Create('You attempted to pick a value from queue but its empty.')
  else
    result:= first.item
end;

//returns the last element in double-edged queue without deleting
function DEQueue<T>.PickLast: T;
begin
  if isEmpty
  then
    raise Exception.Create('You attempted to pick a value from queue but its empty.')
  else
    result:= first.item
end;

// Comparator
function DEQueue<T>.CompareTo(obj: TObject): integer;
begin
  if self.ClassType <> obj.ClassType
  then
    raise EArgumentException.Create('Illegal argument in array elements');
  if self.size > (obj as DEQueue<T>).size
  then
      result:= 1
  else
    if self.size < (obj as DEQueue<T>).size
    then
      result:= -1
    else
      result:= 0
end;

//deletes the double-edged queue
destructor DEQueue<T>.Destroy;
begin
  while not isEmpty
  do
    takeFirst;
  inherited
end;

// creates an empty randomized stack
constructor RandStack<T>.Create;
begin
  inherited;
  SetLength(things, 16);
  size:= 0;
  randomize
end;

// creates a randomized stack witn given initial value
constructor RandStack<T>.Create(thing: T);
begin;
  SetLength(things, 16);
  things[0]:= thing;
  size:= 1;
  randomize
end;

// returns amount of elements in randomized stack
function RandStack<T>.getSize: integer;
begin
  result:= size
end;

// adds specified element in randomized stack
procedure RandStack<T>.put(const thing: T);
begin
  if size > length(things) - 1 then Refactor(size * 2);
  things[size]:= thing;
  size:= size + 1
end;

// takes random element from randomized stack and deletes it
function RandStack<T>.take: T;
var idx: integer;
begin
  idx:= Random(size);
  result:= things[idx];
  if idx <> size - 1 then things[idx]:= things[size - 1];
  size:= size - 1;
  if size < (length(things) div 4) then Refactor(length(things) div 2)
end;

// checks against randomized stack have any element
function RandStack<T>.isEmpty: boolean;
begin
  result:= (size = 0)
end;

// checks against presence of specified element in randomized stack
function RandStack<T>.Contains(thing: T): boolean;
var i: integer;
begin
  result:= false;
  for i:= 0 to size - 1
  do
    if things[i] = thing
    then
    begin
      result:= true;
      Break
    end
end;

// picks a random element from randomized stack without deleting it
function RandStack<T>.pick: T;
begin
  result:= things[random(size)]
end;

procedure RandStack<T>.Refactor(newsize: integer);
var intmd: array of T;
    i: integer;
begin
  SetLength(intmd, newsize);
  for i:= 0 to size - 1
  do
  begin
    intmd[i]:= things[i];
  end;
  things:= intmd
end;

destructor RandStack<T>.Destroy;
var obj: TObject;
begin
  Refactor(1);
  size:= 0;
  inherited
end;

// comparator for randomized stack
function RandStack<T>.CompareTo(obj: TObject): integer;
begin
  if self.ClassType <> obj.ClassType
  then
    raise EArgumentException.Create('Illegal argument in array elements');
  if self.size > (obj as RandStack<T>).size
  then
      result:= 1
  else
    if self.size < (obj as RandStack<T>).size
    then
      result:= -1
    else
      result:= 0
end;

// creates an empty array-stack
constructor AStack<T>.Create;
begin
  inherited;
  SetLength(things, 16);
  size:= 0;
  randomize
end;

// creates an array-stack witn given initial value
constructor AStack<T>.Create(thing: T);
begin;
  SetLength(things, 16);
  things[0]:= thing;
  size:= 1;
  randomize
end;

// returns amount of elements in array-stack
function AStack<T>.getSize: integer;
begin
  result:= size
end;

// adds specified element in randomized stack
procedure AStack<T>.put(const thing: T);
begin
  if size > length(things) - 1 then Refactor(size * 2);
  size:= size + 1;
  things[size - 1]:= thing
end;

// takes first element from array-stack and deletes it
function AStack<T>.take: T;
var idx: integer;
begin
  result:= things[size - 1];
  size:= size - 1;
  if size < (length(things) div 4) then Refactor(length(things) div 2)
end;

// checks against array-stack have any element
function AStack<T>.isEmpty: boolean;
begin
  result:= (size = 0)
end;

// checks against presence of specified element in array-stack
function AStack<T>.Contains(thing: T): boolean;
var i: integer;
begin
  result:= false;
  for i:= 0 to size - 1
  do
    if things[i] = thing
    then
    begin
      result:= true;
      Break
    end
end;

// picks the first element from array-stack without deleting it
function AStack<T>.pick: T;
begin
  result:= things[size - 1]
end;

// picks a random element from array-stack without deleting it
function AStack<T>.pickRand: T;
begin
  result:= things[random(size)]
end;

procedure AStack<T>.Refactor(newsize: integer);
var intmd: array of T;
    i: integer;
begin
  SetLength(intmd, newsize);
  for i:= 0 to size - 1
  do
  begin
    intmd[i]:= things[i];
  end;
  things:= intmd
end;

destructor AStack<T>.Destroy;
var obj: TObject;
begin
  Refactor(1);
  size:= 0;
  inherited
end;

// comparator for randomized stack
function AStack<T>.CompareTo(obj: TObject): integer;
begin
  if self.ClassType <> obj.ClassType
  then
    raise EArgumentException.Create('Illegal argument in array elements');
  if self.size > (obj as AStack<T>).size
  then
      result:= 1
  else
    if self.size < (obj as AStack<T>).size
    then
      result:= -1
    else
      result:= 0
end;

//creates iterator for stack
constructor Stack<T>.Enumerator.Create(stack: Stack<T>);
begin
  self.first:= stack.first;
  idx:= stack.size;
  already:= false;
end;

function Stack<T>.Enumerator.GetCurrent: T;
begin
  result:= self.cur.item
end;

function Stack<T>.Enumerator.MoveNext: Boolean;
begin
  if idx = 0
  then
    result:= false
  else
    if already then
    begin
      self.cur:= self.cur.next;
      dec(idx);
      result:= true
    end
    else
    begin
      self.cur:= self.first;
      already:= true;
      dec(idx);
      result:= true
    end
end;

procedure Stack<T>.Enumerator.Reset;
begin
  self.cur:= self.first
end;

function Stack<T>.GetEnumerator: Enumerator;
begin
  result:= Stack<T>.Enumerator.Create(self)
end;

//creates iterator for queue
constructor Queue<T>.Enumerator.Create(queue: queue<T>);
begin
  self.first:= queue.last;
  idx:= queue.size;
  already:= false;
end;

function Queue<T>.Enumerator.GetCurrent: T;
begin
  result:= self.cur.item
end;

function queue<T>.Enumerator.MoveNext: Boolean;
begin
  if idx = 0
  then
    result:= false
  else
    if already then
    begin
      self.cur:= self.cur.next;
      dec(idx);
      result:= true
    end
    else
    begin
      self.cur:= self.first;
      already:= true;
      dec(idx);
      result:= true
    end
end;

procedure Queue<T>.Enumerator.Reset;
begin
  self.cur:= self.first
end;

function Queue<T>.GetEnumerator: Enumerator;
begin
  result:= Queue<T>.Enumerator.Create(self)
end;

//creates iterator for double-edged queue
constructor DEQueue<T>.Enumerator.Create(dequeue: DEqueue<T>);
begin
  first:= dequeue.first;
  idx:= dequeue.size;
  already:= false;
end;

function DEQueue<T>.Enumerator.GetCurrent: T;
begin
  result:= cur.item
end;

function dequeue<T>.Enumerator.MoveNext: Boolean;
begin
  if idx = 0
  then
    result:= false
  else
    if already then
    begin
      cur:= cur.next;
      dec(idx);
      result:= true
    end
    else
    begin
      cur:= first;
      already:= true;
      dec(idx);
      result:= true
    end
end;

procedure DEQueue<T>.Enumerator.Reset;
begin
  cur:= first
end;

function DEQueue<T>.GetEnumerator: Enumerator;
begin
  result:= DEQueue<T>.Enumerator.Create(self)
end;

// Iterator for randomized stack
function RandStack<T>.GetEnumerator: Enumerator;
begin
  result:= RandStack<T>.Enumerator.Create(self)
end;

constructor RandStack<T>.Enumerator.Create(randst: RandStack<T>);
var hlpr: array of integer;
    i, rnd: integer;
begin
  idx:= -1;
  SetLength(perm, randst.size);
  SetLength(hlpr, randst.size);
  for i:= 0 to randst.size - 1 do hlpr[i]:= i;
  i:= randst.size - 1;
  while i >= 0
  do
  begin
    rnd:= Random(i);
    perm[randst.size - i - 1]:= randst.things[hlpr[rnd]];
    if rnd <> i then hlpr[rnd]:= hlpr[i];
    dec(i)
  end;
end;

function RandStack<T>.Enumerator.GetCurrent: T;
begin
  result:= perm[idx]
end;

function RandStack<T>.Enumerator.MoveNext: boolean;
begin
  if idx = Length(perm) - 1 then result:= false
  else
  begin
    inc(idx);
    result:= true
  end
end;

procedure RandStack<T>.Enumerator.Reset;
begin
  idx:= 0
end;

// Iterator for array-stack
function AStack<T>.GetEnumerator: Enumerator;
begin
  result:= AStack<T>.Enumerator.Create(self)
end;

constructor AStack<T>.Enumerator.Create(ast: AStack<T>);
var i: integer;
begin
  idx:= ast.size;
  iter:= ast.things
end;

function AStack<T>.Enumerator.GetCurrent: T;
begin
  result:= iter[idx]
end;

function AStack<T>.Enumerator.MoveNext: boolean;
begin
  if idx = 0 then result:= false
  else
  begin
    dec(idx);
    result:= true
  end
end;

procedure AStack<T>.Enumerator.Reset;
begin
  idx:= length(iter)
end;

end.
