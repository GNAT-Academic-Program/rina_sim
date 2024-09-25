package RINA.Flow_Allocation is

type State_T is (Idle, Request_Sent, Request_Received, Response_Sent, Flow_Established);
type Decision_T is (No_Decision, Accepted, Denied);

type Flow_Allocator_T is tagged record
   Current_State : State_T := Idle;
   Decision      : Decision_T := No_Decision;
end record;

function Allocate_Flow_Request (FSM : Flow_Allocator_T) return Flow_Allocator_T;
function Receive_Flow_Request  (FSM : Flow_Allocator_T) return Flow_Allocator_T;
function Accept_Flow           (FSM : Flow_Allocator_T) return Flow_Allocator_T;
function Deny_Flow             (FSM : Flow_Allocator_T) return Flow_Allocator_T;
function Send_Flow_Response    (FSM : Flow_Allocator_T) return Flow_Allocator_T;
function Establish_Flow        (FSM : Flow_Allocator_T) return Flow_Allocator_T;
procedure Flow_Allocation_Workflow;

end RINA.RINA.Flow_Allocation;