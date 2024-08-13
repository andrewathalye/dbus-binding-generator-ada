pragma Ada_2005;

with Ada.Containers.Doubly_Linked_Lists;

with D_Bus.Messages;

with dbus_shared_h;

package D_Bus.Support.Message_Handlers is
   ------------------
   -- Message Hash --
   ------------------
   type Message_Hash is new Ada.Containers.Hash_Type;
   function Hash
       (Path       : String;
        Iface      : String;
        Member     : String) return Message_Hash;
   --  TODO incorporate sender
   --  Produce a unique hash given information about a message.

   function Hash
     (Msg : D_Bus.Messages.Message_Type) return Message_Hash;
   --  TODO document
   --  Produce a unique hash given a message.

   -----------------
   -- Message Box --
   -----------------
   No_Cached_Message : exception;
   --  Raised when a user attempts to consume a message
   --  that is not currently in the message box.

   type Message_Reference is record
      Id : Message_Hash;
      Msg : D_Bus.Messages.Message_Type;
   end record;
   package MRL is new Ada.Containers.Doubly_Linked_Lists (Message_Reference);
   --  Internal implementation details for `Message_Box_Type`

   protected type Message_Box_Type is
      procedure Enqueue (Msg : D_Bus.Messages.Message_Type);
      --  Add a message to the box.
      --  `Msg` will be referenced to ensure it is not deallocated.

      procedure Consume
        (Id  :     Message_Hash;
         Msg : out D_Bus.Messages.Message_Type);
      --  Attempt to consume a message with the given hash
      --  from the box.
      --
      --  The caller must unreference `Msg` when done.
      --  Raises `No_Cached_Message` if no match is found.

      procedure Clear
        (Id : Message_Hash);
      --  Remove all messages with the given hash from the box.
      --  These are unreferenced.
      --
      --  It is permissible to call this even if the box doesn’t
      --  contain any matching messages.

      procedure Clear;
      --  Empty the message box. Unreferences all messages within it.
   private
      Data   : MRL.List;
   end Message_Box_Type;
   --  This type is thread-safe: you may share a single message box between
   --  as many threads as desired without corruption. In the case of multiple
   --  messages with the same hash, it is not guaranteed that each thread will
   --  receive the exact message it was expecting (in temporal order). If this
   --  is a concern, instead use a single message box per thread.

   ---------------------
   -- Message Handler --
   ---------------------
   type Message_Handler is access function
      (O : access Root_Object'Class;
       Connection : D_Bus.Connection.Connection_Type;
       Message    : D_Bus.Messages.Message_Type)
      return dbus_shared_h.DBusHandlerResult;

   procedure Register
     (O : access Root_Object'Class; Handler : Message_Handler);
   --  Register object `O` using `Handler`
   --  This allows signals and messages directed at `O` to be received.
   --
   --  ONLY ONE OBJECT WITH A GIVEN PATH CAN BE REGISTERED ON A CONNECTION
   --  If multiple services on a bus offer the same object, select one instance
   --  of that object to receive messages and register it alone.

   procedure Unregister (O : in out Root_Object'Class);
   --  Unregister the previously-registered object `O`
   --  This is called automatically when destroying a registered object.
end D_Bus.Support.Message_Handlers;

