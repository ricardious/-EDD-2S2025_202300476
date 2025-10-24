unit CommunityService;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, BST, Community, SinglyLinkedList, User, UserService, AppState;

type
  TAddCommunityResult = (acrOK, acrEmptyName, acrAlreadyExists);
  TAddMemberResult = (amrOK, amrCommunityNotFound, amrUserNotFound,
    amrAlreadyMember, amrEmpty);
  TPostMessageResult = (pmOK, pmEmpty, pmCommunityNotFound, pmNotMember);

var
  CommunitiesTree: TBSTree = nil;

procedure EnsureCommunitiesTree;

function CompareCommunitiesByName(Data1, Data2: Pointer): integer;
function CommunityToString(Data: Pointer): string;

function AddCommunity(const Name: string; CreatedAt: TDateTime): TAddCommunityResult;
function FindCommunity(const Name: string): PCommunity;
function DeleteCommunity(const Name: string): boolean;
procedure IncCommunityMessageCount(const Name: string);

procedure TraverseCommunitiesInOrder(Process: TProcessNodeProc);
procedure GenerateCommunitiesDot(const FileName: string);

function AddUserToCommunity(const CommunityName, UserEmail: string): TAddMemberResult;
function IsUserMember(C: PCommunity; const UserEmail: string): boolean;
// function RemoveUserFromCommunity(const CommunityName, UserEmail: string): boolean;
function PostMessageToCommunity(
  const CommunityName, AuthorEmail, Content: string): TPostMessageResult;

implementation

procedure EnsureCommunitiesTree;
begin
  if CommunitiesTree = nil then
    CommunitiesTree := TBSTree.Create(@CompareCommunitiesByName);
end;

function CompareCommunitiesByName(Data1, Data2: Pointer): integer;
var
  A, B: PCommunity;
begin
  A := PCommunity(Data1);
  B := PCommunity(Data2);
  Result := AnsiCompareText(A^.Name, B^.Name);
end;

function CommunityToString(Data: Pointer): string;
var
  C: PCommunity;
  membersCount: integer;
begin
  if Data = nil then Exit('(nil)');
  C := PCommunity(Data);
  membersCount := SinglyLinkedList.Count(C^.Members);
  Result := Format('Comunidad: %s\nCreación: %s\nMensajes: %d\nMiembros: %d',
    [C^.Name, FormatDateTime('yyyy-mm-dd', C^.CreatedAt), C^.MessageCount,
    membersCount]);
end;

function AddCommunity(const Name: string; CreatedAt: TDateTime): TAddCommunityResult;
var
  C, Exists: PCommunity;
begin
  EnsureCommunitiesTree;

  if Trim(Name) = '' then Exit(acrEmptyName);

  Exists := FindCommunity(Name);
  if Exists <> nil then Exit(acrAlreadyExists);

  New(C);
  C^.Name := Trim(Name);
  C^.CreatedAt := CreatedAt;
  C^.MessageCount := 0;
  SinglyLinkedList.Init(C^.Members);
  SinglyLinkedList.Init(C^.Messages);
  CommunitiesTree.Insert(C);
  Result := acrOK;
end;

function FindCommunity(const Name: string): PCommunity;
var
  Key: TCommunity;
  Found: Pointer;
begin
  Result := nil;
  EnsureCommunitiesTree;
  Key.Name := Trim(Name);
  Found := CommunitiesTree.Search(@Key);
  if Found <> nil then
    Result := PCommunity(Found);
end;

function DeleteCommunity(const Name: string): boolean;
var
  Key: TCommunity;
begin
  Result := False;
  EnsureCommunitiesTree;
  Key.Name := Trim(Name);
  if CommunitiesTree.Search(@Key) <> nil then
  begin
    CommunitiesTree.Delete(@Key);
    Result := True;
  end;
end;

procedure IncCommunityMessageCount(const Name: string);
var
  C: PCommunity;
begin
  C := FindCommunity(Name);
  if C <> nil then
    Inc(C^.MessageCount);
end;

procedure TraverseCommunitiesInOrder(Process: TProcessNodeProc);
begin
  EnsureCommunitiesTree;
  CommunitiesTree.TraverseInOrder(Process);
end;

procedure GenerateCommunitiesDot(const FileName: string);

  function HtmlEscape(const S: string): string;
  begin
    Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  end;

  procedure EmitRecursive(Node: PBSTNode; Dot: TStrings; var NullIdx: integer);
  var
    Id, NullId: string;
    C: PCommunity;
  begin
    if Node = nil then Exit;

    Id := Format('node%p', [Node]);
    C := PCommunity(Node^.Data);

    Dot.Add(Format('"%s" [shape=plaintext, label=<', [Id]));
    Dot.Add('<TABLE BORDER="1" CELLBORDER="0" CELLPADDING="6" CELLSPACING="0" COLOR="#1e3a8a" BGCOLOR="#0b1220">');
    Dot.Add('<TR>');
    Dot.Add('<TD PORT="L" WIDTH="12" FIXEDSIZE="TRUE"></TD>');
    Dot.Add('<TD>');
    Dot.Add(Format(
      '<FONT POINT-SIZE="12" COLOR="#e5e7eb"><B>%s</B></FONT><BR ALIGN="LEFT"/>',
      [HtmlEscape(C^.Name)]));
    Dot.Add(Format('<FONT COLOR="#94a3b8">Fecha creacion: %s</FONT><BR ALIGN="LEFT"/>',
      [HtmlEscape(FormatDateTime('dd/mm/yyyy', C^.CreatedAt))]));
    Dot.Add(Format('<FONT COLOR="#94a3b8">Mensajes publicados: %d</FONT>',
      [C^.MessageCount]));
    Dot.Add('</TD>');
    Dot.Add('<TD PORT="R" WIDTH="12" FIXEDSIZE="TRUE"></TD>');
    Dot.Add('</TR>');
    Dot.Add('</TABLE>>];');

    if Node^.Left <> nil then
    begin
      Dot.Add(Format('"%s":L -> "node%p";', [Id, Node^.Left]));
      EmitRecursive(Node^.Left, Dot, NullIdx);
    end
    else
    begin
      Inc(NullIdx);
      NullId := Format('null%d', [NullIdx]);
      Dot.Add(Format('"%s" [label="∅", shape=circle, width=0.28, height=0.28, '
        + 'fontsize=9, style=filled, fillcolor="#111827", color="#334155", fontcolor="#64748b"];', [NullId]));
      Dot.Add(Format('"%s":L -> "%s" [style=dashed, color="#334155"];', [Id, NullId]));
    end;

    if Node^.Right <> nil then
    begin
      Dot.Add(Format('"%s":R -> "node%p";', [Id, Node^.Right]));
      EmitRecursive(Node^.Right, Dot, NullIdx);
    end
    else
    begin
      Inc(NullIdx);
      NullId := Format('null%d', [NullIdx]);
      Dot.Add(Format('"%s" [label="∅", shape=circle, width=0.28, height=0.28, '
        + 'fontsize=9, style=filled, fillcolor="#111827", color="#334155", fontcolor="#64748b"];', [NullId]));
      Dot.Add(Format('"%s":R -> "%s" [style=dashed, color="#334155"];', [Id, NullId]));
    end;
  end;

var
  Dot: TStringList;
  NullIdx: integer;
  Root: PBSTNode;
begin
  EnsureCommunitiesTree;
  Dot := TStringList.Create;
  try
    Dot.Add('digraph Communities {');
    Dot.Add('  graph [bgcolor="#0b1220", rankdir=TB, splines=ortho, nodesep=0.5, ranksep=0.6, ordering=out];');
    Dot.Add('  edge  [color="#94a3b8", arrowsize=0.7];');

    Root := CommunitiesTree.GetRoot;
    if Root <> nil then
    begin
      NullIdx := 0;
      EmitRecursive(Root, Dot, NullIdx);
    end;

    Dot.Add('}');
    Dot.SaveToFile(FileName);
  finally
    Dot.Free;
  end;
end;


function IsUserMember(C: PCommunity; const UserEmail: string): boolean;
var
  Node: PSinglyNode;
  U: PUser;
  target: string;
begin
  Result := False;
  if (C = nil) then Exit;
  target := LowerCase(Trim(UserEmail));

  Node := C^.Members.Head;
  while Node <> nil do
  begin
    U := PUser(Node^.Data);
    if SameText(LowerCase(U^.Email), target) then
      Exit(True);
    Node := Node^.Next;
  end;
end;

function AddUserToCommunity(const CommunityName, UserEmail: string): TAddMemberResult;
var
  C: PCommunity;
  U: PUser;
  normEmail, normName: string;
begin
  normName := Trim(CommunityName);
  normEmail := LowerCase(Trim(UserEmail));

  if (normName = '') or (normEmail = '') then Exit(amrEmpty);

  C := FindCommunity(normName);
  if C = nil then Exit(amrCommunityNotFound);

  U := FindUserByEmail(Users, normEmail);
  if U = nil then Exit(amrUserNotFound);

  if IsUserMember(C, normEmail) then Exit(amrAlreadyMember);

  SinglyLinkedList.InsertLast(C^.Members, U);
  Result := amrOK;
end;

// Remove:
// function RemoveUserFromCommunity(const CommunityName, UserEmail: string): boolean;
// var
//   C: PCommunity;
//   U: PUser;
// begin
//   Result := False;
//   C := FindCommunity(CommunityName);
//   if C = nil then Exit;
//   U := FindUserByEmail(Users, LowerCase(Trim(UserEmail)));
//   if U = nil then Exit;
//   SinglyLinkedList.Delete(C^.Members, U);
//   Result := True;
// end;

function PostMessageToCommunity(const CommunityName, AuthorEmail, Content: string):
TPostMessageResult;
var
  C: PCommunity;
  Msg: PCommunityMessage;
  normName, normEmail, body: string;
begin
  normName := Trim(CommunityName);
  normEmail := LowerCase(Trim(AuthorEmail));
  body := Trim(Content);

  if (normName = '') or (body = '') then Exit(pmEmpty);

  C := FindCommunity(normName);
  if C = nil then Exit(pmCommunityNotFound);

  if not IsUserMember(C, normEmail) then
    Exit(pmNotMember);

  New(Msg);
  Msg^.AuthorEmail := normEmail;
  Msg^.Content := body;
  Msg^.PostedAt := Now;

  SinglyLinkedList.InsertLast(C^.Messages, Msg);
  Inc(C^.MessageCount);

  Result := pmOK;
end;


end.
