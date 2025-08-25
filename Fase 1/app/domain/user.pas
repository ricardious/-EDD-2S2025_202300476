unit User;

{$mode ObjFPC}{$H+}

interface

uses
  DoublyLinkedList, Stack, Queue, CircularLinkedList;

type
  PUser = ^TUser;

  TUser = record
    Id: longint;
    Name: ansistring;
    Username: ansistring;
    Password: ansistring;
    Email: ansistring;
    Phone: ansistring;
    Inbox: TDoublyLinkedList;
    Trash: TStack;
    ScheduledMail: TQueue;
    Contacts: TCircularLinkedList;
  end;

implementation

end.
