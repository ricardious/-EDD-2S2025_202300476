unit Community;

{$mode ObjFPC}{$H+}

interface

uses
  SinglyLinkedList;

type
  PCommunityMessage = ^TCommunityMessage;

  TCommunityMessage = record
    AuthorEmail: string;
    Content: string;
    PostedAt: TDateTime;
  end;

  PCommunity = ^TCommunity;

  TCommunity = record
    Name: string;
    CreatedAt: TDateTime;
    MessageCount: integer;
    Members: TSinglyLinkedList;
    Messages: TSinglyLinkedList;
  end;

implementation

end.
