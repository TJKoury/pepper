C$Procedure      F_DWPOOL ( DWPOOL tests )

      SUBROUTINE F_DWPOOL ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB POOL entry point
C     
C        DWPOOL
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB kernel pool
C     utility routine DWPOOL.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 13-JUN-2013 (BVS)
C
C        Set MAXVAR, MAXVAL, MAXAGT, and MXNOTE to values
C        used in POOL.
C
C-    TSPICE Version 1.0.0, 19-MAR-2009 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               LNKNFN

C     
C     Local parameters
C

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )


 
      INTEGER               MAXVAR
      PARAMETER           ( MAXVAR =  26003 )
 
      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 400000 )
 
      INTEGER               MAXAGT
      PARAMETER           ( MAXAGT =   1000 )
 
      INTEGER               MXNOTE
      PARAMETER           ( MXNOTE =  MAXVAR * 5 )

 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )



C
C     Local variables
C

      CHARACTER*(KVNMLN)    AGENT
      CHARACTER*(KVNMLN)    AGENTS ( MXNOTE )
      CHARACTER*(KVNMLN)    AGTSET ( LBCELL : MXNOTE )
      CHARACTER*(KVNMLN)    OKVSET ( LBCELL : MXNOTE )
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(KVNMLN)    VARS   ( MAXVAR )
      CHARACTER*(KVNMLN)    UWAGNT ( MXNOTE )
      CHARACTER*(KVNMLN)    UWVARS ( LBCELL : MAXVAR )
      CHARACTER*(KVNMLN)    XAGSET ( LBCELL : MXNOTE )

      DOUBLE PRECISION      DPVALS ( MAXVAL )

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NAGENT
      INTEGER               NAVAIL
      INTEGER               NSET
      INTEGER               NVARS
      INTEGER               NUSED
      INTEGER               ONAGNT
      INTEGER               ONVARS
      INTEGER               ORDVEC ( MXNOTE )
      INTEGER               UWPOOL ( 2, LBPOOL : MXNOTE )
      INTEGER               UWPTRS ( MAXVAR )

      LOGICAL               UPDATE

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
  
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DWPOOL' )


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Initializations' )

      CALL SSIZEC ( MAXVAR, UWVARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( MAXVAR, UWPTRS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LNKINI ( MXNOTE, UWPOOL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARC ( MAXVAR, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEC ( MXNOTE, AGTSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEC ( MXNOTE, OKVSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEC ( MXNOTE, XAGSET )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C     Find out what's already in the watcher system.
C     Save the original set of watched variables in
C     OKVSET.
C      
      CALL ZZVUPOOL ( OKVSET, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ONVARS = CARDC(OKVSET)

C
C     It's not helpful to save the original agent pool, but
C     we will save the number of entries.
C
      ONAGNT = MXNOTE - LNKNFN(UWPOOL)
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Basic SWPOOL/CVPOOL checks: 1 agent, 1 variable, '
     .//           'variable exists in pool.'                         )

      AGENT     = 'AGENT1'
      NVARS     = 1
      VARS(1)   = 'VAR1'
      DPVALS(1) = 999.D0

      CALL PDPOOL ( VARS(1), 1, DPVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect the agent to receive an update
C     notice as soon as the watch is set.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 0', UPDATE, .TRUE., OK )

C
C     Now the next check should show no update.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 1', UPDATE, .FALSE., OK )
      
C
C     Clear the kernel pool; an update should be 
C     indicated.
C
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 2', UPDATE, .TRUE., OK )

C
C     Now the next check should show no update.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 1', UPDATE, .FALSE., OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Basic SWPOOL/CVPOOL checks: 1 agent, 1 variable, '
     .//           'variable does not exist in pool.'                 )

      AGENT     = 'AGENT2'
      NVARS     = 1
      VARS(1)   = 'VAR2'


      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect the agent to receive an update
C     notice as soon as the watch is set.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 0', UPDATE, .TRUE., OK )

C
C     Now the next check should show no update.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 1', UPDATE, .FALSE., OK )
      
C
C     Clear the kernel pool; an update should be 
C     indicated.
C
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 3', UPDATE, .TRUE., OK )

C
C     Now the next check should show no update.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 4', UPDATE, .FALSE., OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Set one watch and delete it; variable exists.' )


      AGENT     = 'AGENT3'
      NVARS     = 1
      VARS(1)   = 'VAR3'
      DPVALS(1) = -999.D0

      CALL PDPOOL ( VARS(1), 1, DPVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect the agent to receive an update
C     notice as soon as the watch is set. Clear
C     the update notice.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the watch.
C
      CALL DWPOOL ( AGENT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Clear the kernel pool; an update should be 
C     indicated if the watch is still set. We
C     expect that the watch isn't set.
C
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now the next check should show no update.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 0', UPDATE, .FALSE., OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Delete the watches from the previous tests.' )

      CALL CVPOOL ( 'AGENT1', UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DWPOOL ( 'AGENT1' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CVPOOL ( 'AGENT2', UPDATE )
      CALL DWPOOL ( 'AGENT2' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CVPOOL ( 'AGENT3', UPDATE )
      CALL DWPOOL ( 'AGENT3' )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Set multiple watches on one variable; variable '
     .//           'does not exist in the kernel pool.'              )


      VARS(1) = 'VAR1'

      NAGENT = 100

      DO I = 1, NAGENT

         AGENTS(I) = 'AGENT#'
         CALL REPMI ( AGENTS(I), '#', I, AGENTS(I) )

         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Since these watches are new, each agent should be notified
C     of an update. Verify this.
C
      DO I = 1, NAGENT
         
         QNAME = '(0) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(1) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Examine the watcher system; see whether we have the 
C     expected contents.
C      
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the agents for VAR(1).
C
      CALL SSIZEC ( MXNOTE, AGTSET )

      CALL ZZGAPOOL ( VARS(1), UWVARS, UWPTRS, UWPOOL, UWAGNT, AGTSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We must sort the expected agent array before the comparison.
C
      CALL MOVEC  ( AGENTS, NAGENT, XAGSET(1) )

      CALL VALIDC ( MXNOTE, NAGENT, XAGSET )

      CALL CHCKAC ( 'AGTSET', AGTSET(1), '=', XAGSET(1), NAGENT, OK )

C
C     The order of deletion can be important, so we're going to 
C     delete the watches in:
C
C         - order of insertion
C         - reverse order of insertion
C         - alphabetically sorted order
C
C     We'll re-set the watches in the original order each time we
C     re-set them.
C
C     Delete watches in order of insertion.
C
      DO I = 1, NAGENT
 
         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )
      

C
C     Re-set watches in the original order.
C
      DO I = 1, NAGENT
         
         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = '(2) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(3) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO


C
C     Delete watches in the reverse of the order of insertion.
C
      DO I = NAGENT, 1, -1
 
         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


C
C     Re-set watches in the original order.
C
      DO I = 1, NAGENT
         
         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = '(4) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(5) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Delete watches in the reverse of the order of insertion.
C
      DO I = NAGENT, 1, -1
 
         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (2)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


C
C     Re-set watches in the original order.
C
      DO I = 1, NAGENT
         
         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = '(6) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(7) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO


C
C     Delete watches in sorted order.
C
      CALL ORDERC ( AGENTS, NAGENT, ORDVEC )

      DO I = 1, NAGENT
 
         J = ORDVEC(I)

         CALL DWPOOL ( AGENTS(J) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (3)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Set multiple watches on multiple variables; '
     .//           'variables do not exist in the kernel pool. '
     .//           'Each agent watches the same variables.'       )



      NVARS  = 19
      NAGENT = 11

      DO I = 1, NVARS

         VARS(I) = 'VARS#'
         CALL REPMI ( VARS(I), '#', I, VARS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, NAGENT

         AGENTS(I) = 'AGENT#'
         CALL REPMI ( AGENTS(I), '#', I, AGENTS(I) )

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Since these watches are new, each agent should be notified
C     of an update. Verify this.
C
      DO I = 1, NAGENT
         
         QNAME = '(0) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(1) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Examine the watcher system; see whether we have the 
C     expected contents.
C      
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     We must sort the expected agent array before the comparison.
C
      CALL MOVEC  ( AGENTS, NAGENT, XAGSET(1) )

      CALL VALIDC ( MXNOTE, NAGENT, XAGSET )


C
C     Get the agents for each kernel variable; compare these to
C     our sorted agent set.
C
      DO I = 1, NVARS

         CALL ZZGAPOOL ( VARS(I), UWVARS, UWPTRS, 
     .                   UWPOOL,  UWAGNT, AGTSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAC ( 'AGTSET', AGTSET(1), '=', XAGSET(1), NAGENT, OK )

      END DO

C
C     Delete watches in order of insertion.
C
      DO I = 1, NAGENT
 
         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )
      
C
C     Re-set the watches.
C
      DO I = 1, NAGENT

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = '(2) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(3) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Delete watches in the reverse of the order of insertion.
C
      DO I = NAGENT, 1, -1
 
         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


C
C     Re-set the watches.
C
      DO I = 1, NAGENT

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = '(4) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(5) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Delete watches in sorted order.
C
      CALL ORDERC ( AGENTS, NAGENT, ORDVEC )

      DO I = 1, NAGENT
 
         J = ORDVEC(I)

         CALL DWPOOL ( AGENTS(J) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (3)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Set multiple watches on multiple variables; '
     .//           'variables do not exist in the kernel pool. '
     .//           'Each agent watches a different set '
     .//           'of variables.'                               )


C
C     The sets of watched variables are disjoint in this test case.
C     Each variable is watched by only one agent.
C
      NVARS  = 31
      NAGENT = 21


      DO I = 1, NAGENT

         AGENTS(I) = 'AGENT#'
         CALL REPMI ( AGENTS(I), '#', I, AGENTS(I) )

         DO J = 1, NVARS

            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Since these watches are new, each agent should be notified
C     of an update. Verify this.
C
      DO I = 1, NAGENT
         
         QNAME = '(0) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(1) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Examine the watcher system; see whether we have the 
C     expected contents.
C      
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the agent (note: singular) for each kernel variable.
C
      DO I = 1, NAGENT

         DO J = 1, NVARS
C
C           Re-create the name VARS(J).
C
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Get the current watch state.
C
            CALL ZZGAPOOL ( VARS(J), UWVARS, UWPTRS, 
     .                      UWPOOL,  UWAGNT, AGTSET )
            CALL CHCKXC ( .FALSE., ' ', OK )


            QNAME = 'CARDC(AGTSET) (#,#)'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( QNAME, CARDC(AGTSET), '=', 1, 0, OK )

            CALL CHCKSC ( 'AGTSET (0)', AGTSET(1), '=', AGENTS(I), OK )

         END DO

      END DO

C
C     Delete watches in order of insertion.
C
      DO I = 1, NAGENT
 
         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )

 
      
C
C     Re-set the watches.
C
      DO I = 1, NAGENT

         DO J = 1, NVARS

            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = '(2) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(3) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Delete watches in the reverse of the order of insertion.
C
      DO I = NAGENT, 1, -1
 
         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


C
C     Re-set the watches.
C
      DO I = 1, NAGENT

         DO J = 1, NVARS

            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = '(4) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C        Make sure a second test indicates "no update."
C
         QNAME = '(5) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

C
C     Delete watches in sorted order.
C
      CALL ORDERC ( AGENTS, NAGENT, ORDVEC )

      DO I = 1, NAGENT
 
         J = ORDVEC(I)

         CALL DWPOOL ( AGENTS(J) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (2)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Set multiple watches on multiple variables; '
     .//           'variables do not exist in the kernel pool. '
     .//           'Variables are watched by multiple agents.' )


C
C     The sets of watched variables are disjoint in this test case.
C     Each variable is watched by multiple agents.
C
      NSET   =  5
      NVARS  = 21
      NAGENT = 11

      DO I = 1, NSET

         DO J = 1, NVARS
C
C           Each variable name includes I and J.
C           
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO


         DO J = 1, NAGENT
C
C           Each agent name includes I and J.
C           
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Set a watch for the Jth agent on the Ith
C           set of variables.
C
            CALL SWPOOL ( AGENTS(J), NVARS, VARS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Since these watches are new, each agent should be notified
C     of an update. Verify this.
C
      DO I = 1, NSET

         DO J = 1, NAGENT

            QNAME = '(0) Update # #'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CVPOOL ( AGENTS(J), UPDATE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C           Make sure a second test indicates "no update."
C
            QNAME = '(1) Update # #'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CVPOOL ( AGENTS(J), UPDATE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

         END DO

      END DO


C
C     Examine the watcher system; see whether we have the 
C     expected contents.
C      
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the agent (note: singular) for each kernel variable.
C
      DO I = 1, NSET
C
C        Re-create the agent and variable names.
C
         DO J = 1, NVARS
C
C           Each variable name includes I and J.
C           
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Create an agent set while we're creating agent names.
C         
         CALL SCARDC ( 0, XAGSET )

         DO J = 1, NAGENT
C
C           Each agent name includes I and J.
C           
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL INSRTC ( AGENTS(J), XAGSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO



         DO J = 1, NVARS
C
C           Get the current watch state.
C
            CALL ZZGAPOOL ( VARS(J), UWVARS, UWPTRS, 
     .                      UWPOOL,  UWAGNT, AGTSET )
            CALL CHCKXC ( .FALSE., ' ', OK )


            QNAME = 'CARDC(AGTSET) (#,#)'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( QNAME, CARDC(AGTSET), '=', NAGENT, 0, OK )

            CALL CHCKAC ( 'AGTSET (0)', AGTSET(1), '=', 
     .                                  XAGSET(1), NAGENT, OK )

         END DO

      END DO

C
C     Delete watches in order of insertion.
C
      DO I = 1, NSET

         DO J = 1, NAGENT
 
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DWPOOL ( AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


      
C
C     Re-set the watches.
C
      DO I = 1, NSET


         DO J = 1, NVARS
C
C           Each variable name includes I and J.
C           
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO


         DO J = 1, NAGENT
C
C           Each agent name includes I and J.
C           
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Set a watch for the Jth agent on the Ith
C           set of variables.
C
            CALL SWPOOL ( AGENTS(J), NVARS, VARS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Check update status.
C
         DO J = 1, NAGENT

            QNAME = '(2) Update # #'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CVPOOL ( AGENTS(J), UPDATE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C           Make sure a second test indicates "no update."
C
            QNAME = '(3) Update # #'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CVPOOL ( AGENTS(J), UPDATE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

         END DO

      END DO


C
C     Delete watches in the reverse of the order of insertion.
C
      DO I = NSET, 1, -1 

         DO J = NAGENT, 1, -1
 
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DWPOOL ( AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )



C
C     Re-set the watches.
C
C     This time, create a set of all agents.
C
      CALL SCARDC ( 0, AGTSET )

      DO I = 1, NSET

         DO J = 1, NVARS
C
C           Each variable name includes I and J.
C           
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO


         DO J = 1, NAGENT
C
C           Each agent name includes I and J.
C           
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Set a watch for the Jth agent on the Ith
C           set of variables.
C
            CALL SWPOOL ( AGENTS(J), NVARS, VARS )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Add this agent to the agent set.
C
            CALL INSRTC ( AGENTS(J), AGTSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Check update status.
C
         DO J = 1, NAGENT

            QNAME = '(4) Update # #'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CVPOOL ( AGENTS(J), UPDATE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

C
C           Make sure a second test indicates "no update."
C
            QNAME = '(5) Update # #'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CVPOOL ( AGENTS(J), UPDATE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

         END DO

      END DO


C
C     Delete watches in sorted order.
C
      DO I = 1, CARDC( AGTSET )

         CALL DWPOOL ( AGTSET(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C     Check the watch system status.
C     
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


 



C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Try to delete watch with update pending' )


      AGENT     = 'AGENT1'
      NVARS     = 1
      VARS(1)   = 'VAR1'
      DPVALS(1) = -999.D0

      CALL PDPOOL ( VARS(1), 1, DPVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the watch.
C
      CALL DWPOOL ( AGENT )
      CALL CHCKXC ( .TRUE., 'SPICE(UPDATEPENDING)',   OK )

C
C     Clean up the watch.
C
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL DWPOOL ( AGENT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Determine the number of watches set outside of '
     .//           'this test family.'                               )

 
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      NUSED  = CARDC(UWVARS)
      NAVAIL = MAXVAR - NUSED

C
C     Make sure we have enough watches available so the rest
C     of our tests make sense. The exact number of available
C     slots in the watched variable set depends not only on
C     the Toolkit version but the position of this test family
C     in the sequence of TSPICE tests.
C

      CALL CHCKSI ( 'NAVAIL', NAVAIL, '>', 4000, 0, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Kernel variable set overflow: large '
     .//           'variable set'                         )

      DO I = 1, NAVAIL + 1

         VARS(I) = 'VAR(#)'
         CALL REPMI ( VARS(I), '#', I, VARS(I) )

      END DO

      CALL SWPOOL ( 'AGENT1', NAVAIL+1, VARS )
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARSETOVERFLOW)', OK )

C
C     We expect that no new watch was set. Check the watch
C     system.
C
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Kernel variable set overflow: single '
     .//           'variable'                             )

      DO I = 1, NAVAIL

         VARS(I) = 'VAR(#)'
         CALL REPMI ( VARS(I), '#', I, VARS(I) )

         CALL SWPOOL ( 'AGENT1', 1, VARS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     The last straw.
C
      CALL SWPOOL ( 'AGENT1', 1, 'last straw' )
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARSETOVERFLOW)', OK )

C
C     Clean up.
C
      CALL CVPOOL ( 'AGENT1', UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DWPOOL ( 'AGENT1' )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check the watch system.
C
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )

  

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Too many agents for one variable' )

      DO I = 1, MAXAGT

         AGENTS(I) = 'AGENTS(#)'
         CALL REPMI ( AGENTS(I), '#', I, AGENTS(I) )

         CALL SWPOOL ( AGENTS(I), 1, 'VAR1' )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     The last straw.
C
      CALL SWPOOL ( 'last straw', 1, 'VAR1' )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOMANYWATCHES)', OK )

C
C     Clean up.
C
      DO I = 1, MAXAGT

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system.
C
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )

  

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Agent pool overflow' )

C
C     Start out by finding out how much room is in
C     the agent list.
C
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      N = LNKNFN(UWPOOL) / MAXAGT

      DO I = 1, MAXAGT

         VARS(I) = 'VARS(#)'
         CALL REPMI ( VARS(I), '#', I, VARS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, N

         AGENTS(I) = 'AGENTS(#)'
         CALL REPMI ( AGENTS(I), '#', I, AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SWPOOL ( AGENTS(I), MAXAGT, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     The last straw.
C
      CALL SWPOOL ( 'last straw', MAXAGT, VARS )
      CALL CHCKXC ( .TRUE., 'SPICE(AGENTLISTOVERFLOW)', OK )

C
C     Clean up.
C
      DO I = 1, N

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check the watch system.
C
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', 
     .              OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )




      CALL T_SUCCESS ( OK )

      RETURN
      END
