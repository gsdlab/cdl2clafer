pred show {}
run  show for 1

abstract sig c1_ContextAwareCellPhone
{ r_c2_rings : lone c2_rings
, r_c3_incomingCall : lone c43_Call
, r_c4_incomingMessage : lone c45_Message
, r_c5_callAnswerButton : one c5_callAnswerButton
, r_c6_outgoingCall : lone c43_Call
, r_c7_outgoingMessage : lone c45_Message
, r_c8_contactList : one c8_contactList
, r_c10_journal : one c10_journal
, r_c13_batteryLevel : one c13_batteryLevel
, r_c15_auditiveSignal : lone c15_auditiveSignal
, r_c16_answeringMachine : one c16_answeringMachine
, r_c19_autoReplyService : one c19_autoReplyService
, r_c21_clock : one c21_clock
, r_c24_userInMeeting : lone c24_userInMeeting
, r_c25_redirectingToSecretary : lone c25_redirectingToSecretary
, r_c26_outgoingCommunicationCountingService : one c26_outgoingCommunicationCountingService }
{ ((some (this).(@r_c3_incomingCall)) or (some (this).(@r_c4_incomingMessage))) => (some (this).(@r_c2_rings))
  ((some (this).(@r_c24_userInMeeting)) && (some (this).(@r_c3_incomingCall))) => (some (this).(@r_c25_redirectingToSecretary)) }
sig c2_rings
{}
{ one r_c2_rings.this }
sig c5_callAnswerButton
{}
{ one r_c5_callAnswerButton.this }
sig c8_contactList
{ r_c9_contacts : set c36_Contact }
{ one r_c8_contactList.this }
sig c10_journal
{ r_c11_calls : set c43_Call
, r_c12_messages : set c45_Message }
{ one r_c10_journal.this }
sig c13_batteryLevel
{ r_c14_low : lone c14_low }
{ one r_c13_batteryLevel.this }
sig c14_low
{}
{ one r_c14_low.this }
sig c15_auditiveSignal
{}
{ one r_c15_auditiveSignal.this }
sig c16_answeringMachine
{ r_c17_active : lone c17_active
, r_c18_memoryLeft : lone c18_memoryLeft }
{ one r_c16_answeringMachine.this
  ((some (c1_ContextAwareCellPhone).(@r_c3_incomingCall)) && (not (some (this).(@r_c18_memoryLeft)))) => (some (c1_ContextAwareCellPhone).(@r_c15_auditiveSignal)) }
sig c17_active
{}
{ one r_c17_active.this }
sig c18_memoryLeft
{}
{ one r_c18_memoryLeft.this }
sig c19_autoReplyService
{ r_c20_active : lone c20_active }
{ one r_c19_autoReplyService.this }
sig c20_active
{}
{ one r_c20_active.this }
sig c21_clock
{ r_c22_hour : one c22_hour
, r_c23_minute : one c23_minute }
{ one r_c21_clock.this
  ((all cl0 : (this).(@r_c22_hour) | cl0.@ref >= 23) && (all cl0 : (this).(@r_c22_hour) | cl0.@ref =< 8)) => ((some ((c1_ContextAwareCellPhone).(@r_c16_answeringMachine)).(@r_c17_active)) && (some ((c1_ContextAwareCellPhone).(@r_c19_autoReplyService)).(@r_c20_active))) }
sig c22_hour
{ ref : one Int }
{ one r_c22_hour.this
  (all cl0 : ((c1_ContextAwareCellPhone).(@r_c21_clock)).(@r_c22_hour) | cl0.@ref >= 0) && (all cl0 : ((c1_ContextAwareCellPhone).(@r_c21_clock)).(@r_c22_hour) | cl0.@ref =< 23) }
sig c23_minute
{ ref : one Int }
{ one r_c23_minute.this
  (all cl0 : ((c1_ContextAwareCellPhone).(@r_c21_clock)).(@r_c23_minute) | cl0.@ref >= 0) && (all cl0 : ((c1_ContextAwareCellPhone).(@r_c21_clock)).(@r_c23_minute) | cl0.@ref =< 59) }
sig c24_userInMeeting
{}
{ one r_c24_userInMeeting.this }
sig c25_redirectingToSecretary
{}
{ one r_c25_redirectingToSecretary.this }
sig c26_outgoingCommunicationCountingService
{ r_c27_active : lone c27_active
, r_c28_totalCallDuration : one Int
, r_c29_totalNumberOfSentDataPackages : one Int
, r_c30_activeConnection : one c30_activeConnection
, r_c34_WiFiAvailable : lone c34_WiFiAvailable
, r_c35_GPRSAvailable : lone c35_GPRSAvailable }
{ one r_c26_outgoingCommunicationCountingService.this
  (not (some (this).(@r_c27_active))) => ((all cl0 : (this).(@r_c28_totalCallDuration) | cl0.@ref = 0) && (all cl0 : (this).(@r_c29_totalNumberOfSentDataPackages) | cl0.@ref = 0))
  ((((not (some (this).(@r_c34_WiFiAvailable))) && (not (some (this).(@r_c35_GPRSAvailable)))) => (some ((this).(@r_c30_activeConnection)).(@r_c31_mobile))) && ((some (this).(@r_c34_WiFiAvailable)) => (some ((this).(@r_c30_activeConnection)).(@r_c32_WiFi)))) && (((some (this).(@r_c35_GPRSAvailable)) && (not (some ((this).(@r_c30_activeConnection)).(@r_c32_WiFi)))) => (some ((this).(@r_c30_activeConnection)).(@r_c33_GPRS))) }
sig c27_active
{}
{ one r_c27_active.this }
sig c30_activeConnection
{ r_c31_mobile : lone c31_mobile
, r_c32_WiFi : lone c32_WiFi
, r_c33_GPRS : lone c33_GPRS }
{ one r_c30_activeConnection.this
  let children = (r_c31_mobile + r_c32_WiFi + r_c33_GPRS) | one children }
sig c31_mobile
{}
{ one r_c31_mobile.this }
sig c32_WiFi
{}
{ one r_c32_WiFi.this }
sig c33_GPRS
{}
{ one r_c33_GPRS.this }
sig c34_WiFiAvailable
{}
{ one r_c34_WiFiAvailable.this }
sig c35_GPRSAvailable
{}
{ one r_c35_GPRSAvailable.this }
abstract sig c36_Contact
{ r_c37_VIP : lone c37_VIP
, r_c38_canCircumventAnsweringMachine : lone c38_canCircumventAnsweringMachine }
{}
sig c37_VIP
{}
{ one r_c37_VIP.this }
sig c38_canCircumventAnsweringMachine
{}
{ one r_c38_canCircumventAnsweringMachine.this }
abstract sig c39_Communication
{ r_c40_direction : one c40_direction }
{}
sig c40_direction
{ r_c41_incomingFrom : lone c36_Contact
, r_c42_outgoingTo : lone c36_Contact }
{ one r_c40_direction.this
  let children = (r_c41_incomingFrom + r_c42_outgoingTo) | one children }
abstract sig c43_Call extends c39_Communication
{ r_c44_duration : one Int }
{}
one sig c47_Agata extends c36_Contact
{}
abstract sig c45_Message
{ r_c46_contents : one Int }
{}
{ (some (this).(@r_c37_VIP)) && (some (this).(@r_c38_canCircumventAnsweringMachine)) }
one sig c48_CallFromAgata extends c43_Call
{}
{ ((this).(@r_c40_direction)).(@r_c41_incomingFrom) = c47_Agata }
one sig c49_MichalsPhone1 extends c1_ContextAwareCellPhone
{}
{ (((((((this).(@r_c3_incomingCall) = c48_CallFromAgata) && ((c47_Agata) in (((this).(@r_c8_contactList)).(@r_c9_contacts)))) && (some ((this).(@r_c13_batteryLevel)).(@r_c14_low))) && (all cl0 : ((this).(@r_c21_clock)).(@r_c22_hour) | cl0.@ref = 2)) && (all cl0 : ((this).(@r_c21_clock)).(@r_c23_minute) | cl0.@ref = 31)) && (not (some (this).(@r_c24_userInMeeting)))) && (some ((this).(@r_c26_outgoingCommunicationCountingService)).(@r_c34_WiFiAvailable)) }
