
type Protocol_Control_Info is record
   Header    : Access Unsigned_8_Array;
   Length    : Natural;
   -- PCI (Protocol Control Information)
end record;

type EFCP_Configuration is tagged record
   -- Other configuration details will go here
   -- EFCP (Error and Flow Control Protocol)
end record;

type Data_Unit is record
   Configuration   : Access EFCP_Configuration;
   Control_Info    : Protocol_Control_Info;
   SDU_Head        : Access SDU;
   SDU_Tail        : Access SDU;
   Payload_Buffer  : Access SK_Buffer;
   -- SDU (Service Data Unit), SK_Buffer (Socket Buffer)
end record;

type SDU is record
   -- Service Data Unit
   -- Can store data headers or tails
end record;

type SK_Buffer is record
   -- Buffer used to store packet data (like sk_buff in Linux)
end record;

-- Timers
Max_PDU_Lifetime        : constant Duration := 60.0; -- MPL (Maximum PDU Lifetime)
ACK_Timeout             : constant Duration := 5.0;  -- A (ACK Timer)
Retransmission_Timeout  : constant Duration := 10.0; -- R (Retransmission Stop Timer)

Inactivity_Base_Timeout : constant Duration := Max_PDU_Lifetime + ACK_Timeout + Retransmission_Timeout; -- dt (Base Timer)

Sender_Inactivity_Timeout : constant Duration := 3.0 * Inactivity_Base_Timeout;
Receiver_Inactivity_Timeout : constant Duration := 2.0 * Inactivity_Base_Timeout;
-- These handle sender and receiver inactivity based on dt

-- EFCP Container: Manages multiple EFCP instances
type EFCP_Container is tagged record
   EFCP_Instances : Access EFCP_Instance_List;
   -- EFCP: Error and Flow Control Protocol
end record;

-- EFCP Instance: Contains DTP, DTCP, and State Vector Instances
type EFCP_Instance is tagged record
   DTP_Instance      : Access Data_Transfer_Protocol_Instance;
   State_Vector      : Access State_Vector_Instance;
   DTCP_Instance     : Access Data_Transfer_Control_Protocol_Instance;
   -- DTP: Data Transfer Protocol
   -- DTCP: Data Transfer Control Protocol
end record;

-- List of EFCP Instances (to support multiple instances in the container)
type EFCP_Instance_List is array (Positive range <>) of EFCP_Instance;

-- Data Transfer Protocol (DTP) Instance
type Data_Transfer_Protocol_Instance is tagged record
   CWQ                  : Access Congestion_Window_Queue;
   SeqQ                 : Access Sequence_Queue;
   DTP_Policy_Set       : Access DTP_Policy_Set;
   ACK_Timer            : Duration;
   -- CWQ: Congestion Window Queue
   -- SeqQ: Sequence Queue
   -- ACK_Timer (A): Timer for ACK management
end record;

-- Congestion Window Queue (CWQ)
type Congestion_Window_Queue is tagged record
   -- Logic for managing congestion window queue goes here
end record;

-- Sequence Queue (SeqQ)
type Sequence_Queue is tagged record
   -- Logic for managing sequence order goes here
end record;

-- DTP Policy Set: Defines DTP-related policies
type DTP_Policy_Set is tagged record
   -- Policy set governing the DTP instance behavior
end record;

-- State Vector: Tracks sending and receiving state windows
type State_Vector_Instance is tagged record
   Sender_Last_Window_Edge    : Natural;  -- sndrLWE: Sender Last Window Edge
   Receiver_Right_Window_Edge : Natural;  -- rcvrRWE: Receiver Right Window Edge
   -- Manages the sending and receiving state
end record;

-- Data Transfer Control Protocol (DTCP) Instance
type Data_Transfer_Control_Protocol_Instance is tagged record
   RTX_Queue            : Access Retransmission_Queue;
   DTCP_Policy_Set      : Access DTCP_Policy_Set;
   Retransmission_Timer : Duration;
   -- RTXQ: Retransmission Queue
   -- Rtx_Timer (R): Timer to control retransmission retries
end record;

-- Retransmission Queue (RTXQ)
type Retransmission_Queue is tagged record
   -- Logic for managing retransmissions goes here
end record;

-- DTCP Policy Set: Defines DTCP-related policies
type DTCP_Policy_Set is tagged record
   -- Policy set governing the DTCP instance behavior
end record;


```ada
-- Data Transfer Control PCI (Protocol Control Information) Packet
-- Used for managing control-related information during data transfers
type Control_PDU is record
   Destination_Address       : Address_Type;      -- Dst_Addr: Destination Address
   Source_Address            : Address_Type;      -- Src_Addr: Source Address
   Destination_CEP_Id        : CEP_Id_Type;       -- Dst_CEP_Id: Destination CEP Identifier
   Source_CEP_Id             : CEP_Id_Type;       -- Src_CEP_Id: Source CEP Identifier
   Quality_Of_Service_Id     : QoS_Id_Type;       -- QoS_Id: Quality of Service Identifier
   PDU_Type                  : PDU_Type_Type;     -- PDU_Type: Type of Protocol Data Unit
   Control_Sequence_Number   : Natural;           -- Ctrl_Seq_Num: Control Sequence Number
   Sender_Right_Window_Edge  : Natural;           -- sndRWE: Sender's Right Window Edge
   Receiver_Right_Window_Edge: Natural;           -- rcvRWE: Receiver's Right Window Edge
   Sender_Last_Window_Edge   : Natural;           -- sndLWE: Sender's Last Window Edge
end record;

-- Data Transfer PCI (Protocol Control Information) Packet
-- Used for transferring actual data payloads with additional flags
type Data_PDU is record
   Destination_Address   : Address_Type;        -- Dst_Addr: Destination Address
   Source_Address        : Address_Type;        -- Src_Addr: Source Address
   Destination_CEP_Id    : CEP_Id_Type;         -- Dst_CEP_Id: Destination CEP Identifier
   Source_CEP_Id         : CEP_Id_Type;         -- Src_CEP_Id: Source CEP Identifier
   Quality_Of_Service_Id : QoS_Id_Type;         -- QoS_Id: Quality of Service Identifier
   PDU_Type              : PDU_Type_Type;       -- PDU_Type: Type of Protocol Data Unit
   Sequence_Number       : Natural;             -- Seq_Num: Sequence Number for this data packet
   Data_Reliability_Flag : Boolean;             -- DRF: Data Reliability Flag (ensures delivery)
   Explicit_Congestion_Notification : Boolean;  -- ECN: Explicit Congestion Notification
end record;

-- Supporting Types
subtype Address_Type is Natural;
subtype CEP_Id_Type is Natural;
subtype QoS_Id_Type is Natural;
subtype PDU_Type_Type is Natural;

-- Explanation of key fields:
-- Destination_Address (Dst_Addr), Source_Address (Src_Addr): Identifies source and destination endpoints
-- Destination_CEP_Id (Dst_CEP_Id), Source_CEP_Id (Src_CEP_Id): Connection End Point Identifiers
-- Quality_Of_Service_Id (QoS_Id): Defines service quality (e.g., latency, bandwidth)
-- PDU_Type: Specifies the type of PDU (data, control, etc.)
-- Control_Sequence_Number (Ctrl_Seq_Num): Sequence number for control PDUs
-- Sequence_Number (Seq_Num): Sequence number for data PDUs
-- Data_Reliability_Flag (DRF): Flag for ensuring reliable data transmission
-- Explicit_Congestion_Notification (ECN): Flag for notifying congestion in the network
-- Sender_Right_Window_Edge (sndRWE): Right edge of sender's window in control PDUs
-- Receiver_Right_Window_Edge (rcvRWE): Right edge of receiver's window in control PDUs
-- Sender_Last_Window_Edge (sndLWE): Last window edge to control retransmissions


-- Slide: Implementation of EFCP Functions

-- The EFCP (Error and Flow Control Protocol) must operate across DIFs (Distributed IPC Facilities)
-- These DIFs can vary greatly, so the mechanisms must be decoupled and usable independently.

-- EFCP Instance
type EFCP_Instance is tagged record
   DTP_Instance   : Data_Transfer_Protocol_Instance;
   DTCP_Instance  : Data_Transfer_Control_Protocol_Instance;
end record;

-- Data Transfer Protocol (DTP)
-- Handles:
--  - Sequence Numbers (to ensure ordered delivery)
--  - Ordering (maintains correct packet order)
--  - No duplications (ensures packets aren't duplicated)
type Data_Transfer_Protocol_Instance is tagged record
   Sequence_Number     : Natural;  -- Keeps track of sequence numbers
   Ordered_Delivery    : Boolean;  -- Ensures packets are delivered in order
   Duplicate_Check     : Boolean;  -- Ensures no duplicate packets are accepted
end record;

-- Data Transfer Control Protocol (DTCP)
-- Handles:
--  - Flow Control (manages how much data can be sent without overwhelming the receiver)
--  - Retransmission Control (handles retransmissions for reliability)
--  - Congestion Control (prevents network congestion)
type Data_Transfer_Control_Protocol_Instance is tagged record
   Flow_Control_Enabled   : Boolean;  -- Flow control enabled or not
   Retransmission_Control : Boolean;  -- Controls retransmission mechanism
   Congestion_Control     : Boolean;  -- Manages congestion control
end record;

-- The decoupled nature of the EFCP instance allows independent control over DTP and DTCP functions,
-- making it adaptable to different DIFs while maintaining flow and congestion control.


-- EFCP Container: Manages multiple EFCP instances
type EFCP_Container is tagged record
   EFCP_Instances : Access EFCP_Instance_List; -- List of EFCP instances
end record;

-- EFCP Instance: Represents an active connection
type EFCP_Instance is tagged record
   DTP_Instance    : Access Data_Transfer_Protocol_Instance;  -- DTP: Data Transfer Protocol instance
   State_Vector    : Access State_Vector_Instance;            -- State Vector for flow control
   DTCP_Instance   : Access Data_Transfer_Control_Protocol_Instance; -- DTCP: Data Transfer Control Protocol instance
end record;

-- Data Transfer Protocol (DTP)
-- Handles data packet sequence numbers, ordering, and ensures no duplications
type Data_Transfer_Protocol_Instance is tagged record
   Congestion_Window_Queue  : Access Congestion_Window_Queue; -- Manages congestion control
   Sequence_Queue           : Access Sequence_Queue;          -- Ensures ordered packet delivery
   DTP_Policy_Set           : Access DTP_Policy_Set;          -- Policy set controlling DTP behavior
   Acknowledgment_Timer     : Duration;                       -- Timer for managing ACKs
end record;

-- Congestion Window Queue
-- Tracks how much data can be sent without overloading the network
type Congestion_Window_Queue is tagged record
   -- Logic for managing the congestion window goes here
end record;

-- Sequence Queue
-- Ensures that packets are processed in the correct sequence
type Sequence_Queue is tagged record
   -- Logic for packet sequencing and ordering
end record;

-- DTP Policy Set: Defines policies for data transfer protocol behavior
type DTP_Policy_Set is tagged record
   -- Set of policies controlling the DTP instance
end record;

-- State Vector: Tracks flow control information like sender/receiver window edges
type State_Vector_Instance is tagged record
   Sender_Last_Window_Edge    : Natural;  -- Last window edge for sender (sndLWE)
   Receiver_Right_Window_Edge : Natural;  -- Right window edge for receiver (rcvrRWE)
end record;

-- Data Transfer Control Protocol (DTCP)
-- Handles flow control, retransmissions, and congestion management
type Data_Transfer_Control_Protocol_Instance is tagged record
   Retransmission_Queue       : Access Retransmission_Queue;   -- Queue for handling packet retransmissions
   DTCP_Policy_Set            : Access DTCP_Policy_Set;        -- Policy set controlling DTCP behavior
   Retransmission_Timer       : Duration;                      -- Timer for managing retransmissions
end record;

-- Retransmission Queue
-- Manages packets that need to be retransmitted
type Retransmission_Queue is tagged record
   -- Logic for managing retransmissions
end record;

-- DTCP Policy Set: Defines policies for DTCP behavior
type DTCP_Policy_Set is tagged record
   -- Set of policies controlling the DTCP instance
end record;

-- Procedure for creating an EFCP connection
procedure Create_EFCP_Connection is
begin
   -- Logic to establish EFCP connection
end Create_EFCP_Connection;

-- Procedure for destroying an EFCP connection
procedure Destroy_EFCP_Connection is
begin
   -- Logic to tear down EFCP connection
end Destroy_EFCP_Connection;

-- Procedure for writing to the EFCP container (incoming buffer)
procedure EFCP_Container_Write(Data_Buffer : in Network_Buffer) is
begin
   -- Logic for writing incoming data into EFCP container
end EFCP_Container_Write;

-- Procedure for enqueuing data into the IPC Process or Kernel Forwarding Agent
procedure IPC_Enqueue(Data_Buffer : in Network_Buffer; Port_ID : in Natural) is
begin
   -- Logic to enqueue data to the Inter-Process Communication Process (IPCP) or Kernel Forwarding Agent (KFA)
end IPC_Enqueue;

-- Procedure for receiving data from the network (handled by the RMT instance)
procedure Receive_From_Network(Data_Buffer : in Network_Buffer) is
begin
   -- Logic to handle data reception from the Relay Message Transfer (RMT) instance
end Receive_From_Network;

-- Procedure for sending data to the network (through the RMT instance)
procedure Send_To_Network(Data_Buffer : in Network_Buffer) is
begin
   -- Logic to send data through the Relay Message Transfer (RMT) instance
end Send_To_Network;

-- Supporting Types

-- Network_Buffer: Represents a data buffer for incoming/outgoing packets
-- (Similar to Linux's `sk_buff` structure)
type Network_Buffer is record
   -- Logic for managing the network data buffer
end record;

-- List of EFCP Instances: To support multiple instances in the container
type EFCP_Instance_List is array (Positive range <>) of EFCP_Instance;


-- EFCP Container: Manages multiple EFCP instances
type EFCP_Container is tagged record
   EFCP_Instances : Access EFCP_Instance_List; -- List of EFCP instances
end record;

-- EFCP Instance: Represents an active connection
type EFCP_Instance is tagged record
   DTP_Instance    : Access Data_Transfer_Protocol_Instance;  -- DTP: Data Transfer Protocol instance
   State_Vector    : Access State_Vector_Instance;            -- State Vector for tracking flow control and window edges
   DTCP_Instance   : Access Data_Transfer_Control_Protocol_Instance; -- DTCP: Data Transfer Control Protocol instance
end record;

-- Data Transfer Protocol (DTP)
-- Handles data packet sequence numbers, ordering, and ensures no duplications
type Data_Transfer_Protocol_Instance is tagged record
   Congestion_Window_Queue  : Access Congestion_Window_Queue; -- Manages congestion control
   Sequence_Queue           : Access Sequence_Queue;          -- Ensures ordered packet delivery
   DTP_Policy_Set           : Access DTP_Policy_Set;          -- Policy set controlling DTP behavior
   Acknowledgment_Timer     : Duration;                       -- Timer for managing ACKs
end record;

-- Congestion Window Queue
-- Tracks how much data can be sent without overloading the network
type Congestion_Window_Queue is tagged record
   -- Logic for managing the congestion window goes here
end record;

-- Sequence Queue
-- Ensures that packets are processed in the correct sequence
type Sequence_Queue is tagged record
   -- Logic for packet sequencing and ordering
end record;

-- DTP Policy Set: Defines policies for data transfer protocol behavior
type DTP_Policy_Set is tagged record
   -- Set of policies controlling the DTP instance
end record;

-- State Vector: Tracks flow control window edges (sndLWE, rcvrRWE)
type State_Vector_Instance is tagged record
   Sender_Last_Window_Edge     : Natural;  -- sndrLWE: Sender's Last Window Edge, tracks the last byte sent
   Receiver_Right_Window_Edge  : Natural;  -- rcvrRWE: Receiver's Right Window Edge, tracks the rightmost byte received
   Sender_Right_Window_Edge    : Natural;  -- sndrRWE: Sender's Right Window Edge, indicates the last byte allowed to be sent
end record;

-- Data Transfer Control Protocol (DTCP)
-- Handles flow control, retransmissions, and congestion management
type Data_Transfer_Control_Protocol_Instance is tagged record
   Retransmission_Queue       : Access Retransmission_Queue;   -- RTXQ: Retransmission Queue
   DTCP_Policy_Set            : Access DTCP_Policy_Set;        -- Policy set controlling DTCP behavior
   Retransmission_Timer       : Duration;                      -- Timer for managing retransmissions
end record;

-- Retransmission Queue
-- Manages packets that need to be retransmitted
type Retransmission_Queue is tagged record
   -- Logic for managing retransmissions
end record;

-- DTCP Policy Set: Defines policies for DTCP behavior
type DTCP_Policy_Set is tagged record
   -- Set of policies controlling the DTCP instance
end record;

-- Procedure for creating an EFCP connection
procedure Create_EFCP_Connection is
begin
   -- Logic to establish EFCP connection
end Create_EFCP_Connection;

-- Procedure for destroying an EFCP connection
procedure Destroy_EFCP_Connection is
begin
   -- Logic to tear down EFCP connection
end Destroy_EFCP_Connection;

-- Procedure for writing to the EFCP container (incoming buffer)
procedure EFCP_Container_Write(Data_Buffer : in Network_Buffer) is
begin
   -- Logic for writing incoming data into EFCP container
end EFCP_Container_Write;

-- Procedure for enqueuing data into the IPC Process or Kernel Forwarding Agent
procedure IPC_Enqueue(Data_Buffer : in Network_Buffer; Port_ID : in Natural) is
begin
   -- Logic to enqueue data to the Inter-Process Communication Process (IPCP) or Kernel Forwarding Agent (KFA)
end IPC_Enqueue;

-- Procedure for receiving data from the network (handled by the RMT instance)
procedure Receive_From_Network(Data_Buffer : in Network_Buffer) is
begin
   -- Logic to handle data reception from the Relay Message Transfer (RMT) instance
end Receive_From_Network;

-- Procedure for sending data to the network (through the RMT instance)
procedure Send_To_Network(Data_Buffer : in Network_Buffer) is
begin
   -- Logic to send data through the Relay Message Transfer (RMT) instance
end Send_To_Network;

-- Supporting Types

-- Network_Buffer: Represents a data buffer for incoming/outgoing packets
-- (Similar to Linux's `sk_buff` structure)
type Network_Buffer is record
   -- Logic for managing the network data buffer
end record;

-- List of EFCP Instances: To support multiple instances in the container
type EFCP_Instance_List is array (Positive range <>) of EFCP_Instance;


-- Define the states of the FSM
type Policy_Set_State is (Unpublished, Published, Created, Controlled, Reconciled, Destroyed);

-- Define the FSM for the EFCP Policy Set
type EFCP_Policy_Set_FSM is tagged record
   Current_State : Policy_Set_State := Unpublished;
end record;

-- Procedure to publish the policy set
procedure Publish_Policy_Set(FSM : in out EFCP_Policy_Set_FSM) is
begin
   if FSM.Current_State = Unpublished then
      -- Transition from Unpublished to Published
      FSM.Current_State := Published;
      Put_Line("Policy Set Published (dtp_ps_publish)");
   else
      Put_Line("Invalid transition: Can only publish from Unpublished state");
   end if;
end Publish_Policy_Set;

-- Procedure to create the policy set
procedure Create_Policy_Set(FSM : in out EFCP_Policy_Set_FSM) is
begin
   if FSM.Current_State = Published then
      -- Transition from Published to Created
      FSM.Current_State := Created;
      Put_Line("Policy Set Created");
   else
      Put_Line("Invalid transition: Can only create from Published state");
   end if;
end Create_Policy_Set;

-- Procedure to handle transmission control
procedure Control_Transmission(FSM : in out EFCP_Policy_Set_FSM) is
begin
   if FSM.Current_State = Created then
      -- Transition from Created to Controlled
      FSM.Current_State := Controlled;
      Put_Line("Transmission Controlled");
   else
      Put_Line("Invalid transition: Can only control transmission from Created state");
   end if;
end Control_Transmission;

-- Procedure to reconcile conflicts
procedure Reconcile_Conflict(FSM : in out EFCP_Policy_Set_FSM) is
begin
   if FSM.Current_State = Controlled then
      -- Transition from Controlled to Reconciled
      FSM.Current_State := Reconciled;
      Put_Line("Conflicts Reconciled");
   else
      Put_Line("Invalid transition: Can only reconcile from Controlled state");
   end if;
end Reconcile_Conflict;

-- Procedure to destroy the policy set
procedure Destroy_Policy_Set(FSM : in out EFCP_Policy_Set_FSM) is
begin
   if FSM.Current_State = Reconciled then
      -- Transition from Reconciled to Destroyed
      FSM.Current_State := Destroyed;
      Put_Line("Policy Set Destroyed");
   elsif FSM.Current_State = Published then
      -- Special case: Can destroy directly from Published state
      FSM.Current_State := Destroyed;
      Put_Line("Policy Set Destroyed");
   else
      Put_Line("Invalid transition: Can only destroy from Reconciled or Published state");
   end if;
end Destroy_Policy_Set;

-- Procedure to unpublish the policy set
procedure Unpublish_Policy_Set(FSM : in out EFCP_Policy_Set_FSM) is
begin
   if FSM.Current_State = Destroyed then
      -- Transition from Destroyed back to Unpublished
      FSM.Current_State := Unpublished;
      Put_Line("Policy Set Unpublished (dtp_ps_unpublish)");
   else
      Put_Line("Invalid transition: Can only unpublish from Destroyed state");
   end if;
end Unpublish_Policy_Set;

-- Main procedure to demonstrate FSM
procedure EFCP_Workflow is
   Policy_FSM : EFCP_Policy_Set_FSM;
begin
   -- Simulating the workflow according to the diagram
   Publish_Policy_Set(Policy_FSM);
   Create_Policy_Set(Policy_FSM);
   Control_Transmission(Policy_FSM);
   Reconcile_Conflict(Policy_FSM);
   Destroy_Policy_Set(Policy_FSM);
   Unpublish_Policy_Set(Policy_FSM);
end EFCP_Workflow;






-- Define the states for flow allocation
type Flow_Allocation_State is (Idle, Request_Sent, Request_Received, Response_Sent, Flow_Established);

-- Define a decision type for accept/deny
type Flow_Decision is (No_Decision, Accepted, Denied);

-- Define the FSM for Flow Allocation with a decision tracker
type Flow_Allocator_FSM is tagged record
   Current_State : Flow_Allocation_State := Idle;
   Decision      : Flow_Decision := No_Decision; -- Tracks the decision made by App B
end record;

-- Procedure for initiating a flow request
procedure Allocate_Flow_Request(FSM : in out Flow_Allocator_FSM) is
begin
   if FSM.Current_State = Idle then
      -- Transition from Idle to Request Sent
      FSM.Current_State := Request_Sent;
      Put_Line("Flow Request Sent from Host 1 (App A)");
   else
      Put_Line("Invalid transition: Can only send request from Idle state");
   end if;
end Allocate_Flow_Request;

-- Procedure for receiving a flow request (on Host 2)
procedure Receive_Flow_Request(FSM : in out Flow_Allocator_FSM) is
begin
   if FSM.Current_State = Request_Sent then
      -- Transition from Request Sent to Request Received
      FSM.Current_State := Request_Received;
      Put_Line("Flow Request Received on Host 2 (IPCP)");
   else
      Put_Line("Invalid transition: Can only receive request after it is sent");
   end if;
end Receive_Flow_Request;

-- Procedure for accepting the flow request (App B)
procedure Accept_Flow(FSM : in out Flow_Allocator_FSM) is
begin
   if FSM.Current_State = Request_Received then
      -- Transition from Request Received to Response Sent
      FSM.Current_State := Response_Sent;
      FSM.Decision := Accepted; -- Track that the flow was accepted
      Put_Line("Flow Request Accepted by App B");
   else
      Put_Line("Invalid transition: Can only accept from Request Received state");
   end if;
end Accept_Flow;

-- Procedure for denying the flow request (App B)
procedure Deny_Flow(FSM : in out Flow_Allocator_FSM) is
begin
   if FSM.Current_State = Request_Received then
      -- Transition from Request Received to Response Sent
      FSM.Current_State := Response_Sent;
      FSM.Decision := Denied; -- Track that the flow was denied
      Put_Line("Flow Request Denied by App B");
   else
      Put_Line("Invalid transition: Can only deny from Request Received state");
   end if;
end Deny_Flow;

-- Procedure for sending the flow response
procedure Send_Flow_Response(FSM : in out Flow_Allocator_FSM) is
begin
   if FSM.Current_State = Response_Sent then
      -- Response has already been sent; this just confirms the response
      if FSM.Decision = Accepted then
         Put_Line("Flow Response Sent: Flow Accepted");
      elsif FSM.Decision = Denied then
         Put_Line("Flow Response Sent: Flow Denied");
      end if;
   else
      Put_Line("Invalid transition: Can only send response after flow is accepted or denied");
   end if;
end Send_Flow_Response;

-- Procedure for establishing the flow (if accepted)
procedure Establish_Flow(FSM : in out Flow_Allocator_FSM) is
begin
   if FSM.Decision = Accepted and FSM.Current_State = Response_Sent then
      -- Transition from Response Sent to Flow Established
      FSM.Current_State := Flow_Established;
      Put_Line("Flow Established between Host 1 and Host 2");
   else
      Put_Line("Invalid transition: Can only establish flow if it was accepted");
   end if;
end Establish_Flow;

-- Main procedure to demonstrate FSM
procedure Flow_Allocation_Workflow is
   Flow_FSM : Flow_Allocator_FSM;
begin
   -- Simulating the workflow according to the diagram
   Allocate_Flow_Request(Flow_FSM);
   Receive_Flow_Request(Flow_FSM);
   Accept_Flow(Flow_FSM);       -- or Deny_Flow(Flow_FSM); depending on the scenario
   Send_Flow_Response(Flow_FSM);
   Establish_Flow(Flow_FSM);
end Flow_Allocation_Workflow;


