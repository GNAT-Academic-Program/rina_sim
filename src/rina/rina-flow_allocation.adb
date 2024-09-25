package body RINA.Flow_Allocation is

   -- Procedure for initiating a flow request
   function Allocate_Flow_Request (FSM : Flow_Allocator_T) return Flow_Allocator_T is
      (if FSM.Current_State = Idle then (Request_Sent, FSM.Decision) else Flow_Allocator_T);

   -- Procedure for receiving a flow request (on Host 2)
   procedure Receive_Flow_Request (FSM : in out Flow_Allocator_FSM) is
   begin
      if FSM.Current_State = Request_Sent then
         -- Transition from Request Sent to Request Received
         FSM.Current_State := Request_Received;
         Put_Line ("Flow Request Received on Host 2 (IPCP)");
      else
         Put_Line
           ("Invalid transition: Can only receive request after it is sent");
      end if;
   end Receive_Flow_Request;

   -- Procedure for accepting the flow request (App B)
   procedure Accept_Flow (FSM : in out Flow_Allocator_FSM) is
   begin
      if FSM.Current_State = Request_Received then
         -- Transition from Request Received to Response Sent
         FSM.Current_State := Response_Sent;
         FSM.Decision      := Accepted; -- Track that the flow was accepted
         Put_Line ("Flow Request Accepted by App B");
      else
         Put_Line
           ("Invalid transition: Can only accept from Request Received state");
      end if;
   end Accept_Flow;

   -- Procedure for denying the flow request (App B)
   procedure Deny_Flow (FSM : in out Flow_Allocator_FSM) is
   begin
      if FSM.Current_State = Request_Received then
         -- Transition from Request Received to Response Sent
         FSM.Current_State := Response_Sent;
         FSM.Decision      := Denied; -- Track that the flow was denied
         Put_Line ("Flow Request Denied by App B");
      else
         Put_Line
           ("Invalid transition: Can only deny from Request Received state");
      end if;
   end Deny_Flow;

   -- Procedure for sending the flow response
   procedure Send_Flow_Response (FSM : in out Flow_Allocator_FSM) is
   begin
      if FSM.Current_State = Response_Sent then
         -- Response has already been sent; this just confirms the response
         if FSM.Decision = Accepted then
            Put_Line ("Flow Response Sent: Flow Accepted");
         elsif FSM.Decision = Denied then
            Put_Line ("Flow Response Sent: Flow Denied");
         end if;
      else
         Put_Line
           ("Invalid transition: Can only send response after flow is accepted or denied");
      end if;
   end Send_Flow_Response;

   -- Procedure for establishing the flow (if accepted)
   procedure Establish_Flow (FSM : in out Flow_Allocator_FSM) is
   begin
      if FSM.Decision = Accepted and FSM.Current_State = Response_Sent then
         FSM.Current_State := Flow_Established;
         Put_Line ("Flow Established between Host 1 and Host 2");
      else
         Put_Line
           ("Invalid transition: Can only establish flow if it was accepted");
      end if;
   end Establish_Flow;

   -- Main procedure to demonstrate FSM
   procedure Flow_Allocation_Workflow is
      Flow_FSM : Flow_Allocator_FSM;
   begin
      -- Simulating the workflow according to the diagram
      Allocate_Flow_Request (Flow_FSM);
      Receive_Flow_Request (Flow_FSM);
      Accept_Flow (Flow_FSM);
      Send_Flow_Response (Flow_FSM);
      Establish_Flow (Flow_FSM);
   end Flow_Allocation_Workflow;

end RINA.Flow_Allocation;
