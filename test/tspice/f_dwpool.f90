!$Procedure      F_DWPOOL ( DWPOOL tests )

      SUBROUTINE F_DWPOOL ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB POOL entry point
!
!        DWPOOL
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB kernel pool
!     utility routine DWPOOL.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 13-JUN-2013 (BVS)
!
!        Set MAXVAR, MAXVAL, MAXAGT, and MXNOTE to values
!        used in POOL.
!
!-    TSPICE Version 1.0.0, 19-MAR-2009 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               CARDC
      INTEGER               LNKNFN

!
!     Local parameters
!

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



!
!     Local variables
!

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

!
!     Saved variables
!
      SAVE

!
!     Initial values
!

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DWPOOL' )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
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


!
!     Find out what's already in the watcher system.
!     Save the original set of watched variables in
!     OKVSET.
!
      CALL ZZVUPOOL ( OKVSET, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ONVARS = CARDC(OKVSET)

!
!     It's not helpful to save the original agent pool, but
!     we will save the number of entries.
!
      ONAGNT = MXNOTE - LNKNFN(UWPOOL)
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Basic SWPOOL/CVPOOL checks: 1 agent, 1 variable, ' &
      //           'variable exists in pool.'                         )

      AGENT     = 'AGENT1'
      NVARS     = 1
      VARS(1)   = 'VAR1'
      DPVALS(1) = 999.D0

      CALL PDPOOL ( VARS(1), 1, DPVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect the agent to receive an update
!     notice as soon as the watch is set.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 0', UPDATE, .TRUE., OK )

!
!     Now the next check should show no update.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 1', UPDATE, .FALSE., OK )

!
!     Clear the kernel pool; an update should be
!     indicated.
!
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 2', UPDATE, .TRUE., OK )

!
!     Now the next check should show no update.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 1', UPDATE, .FALSE., OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Basic SWPOOL/CVPOOL checks: 1 agent, 1 variable, ' &
      //           'variable does not exist in pool.'                 )

      AGENT     = 'AGENT2'
      NVARS     = 1
      VARS(1)   = 'VAR2'


      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect the agent to receive an update
!     notice as soon as the watch is set.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 0', UPDATE, .TRUE., OK )

!
!     Now the next check should show no update.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 1', UPDATE, .FALSE., OK )

!
!     Clear the kernel pool; an update should be
!     indicated.
!
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 3', UPDATE, .TRUE., OK )

!
!     Now the next check should show no update.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 4', UPDATE, .FALSE., OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Set one watch and delete it; variable exists.' )


      AGENT     = 'AGENT3'
      NVARS     = 1
      VARS(1)   = 'VAR3'
      DPVALS(1) = -999.D0

      CALL PDPOOL ( VARS(1), 1, DPVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect the agent to receive an update
!     notice as soon as the watch is set. Clear
!     the update notice.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the watch.
!
      CALL DWPOOL ( AGENT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Clear the kernel pool; an update should be
!     indicated if the watch is still set. We
!     expect that the watch isn't set.
!
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now the next check should show no update.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Update 0', UPDATE, .FALSE., OK )

!
!---- Case -------------------------------------------------------------
!
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


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Set multiple watches on one variable; variable ' &
      //           'does not exist in the kernel pool.'              )


      VARS(1) = 'VAR1'

      NAGENT = 100

      DO I = 1, NAGENT

         AGENTS(I) = 'AGENT#'
         CALL REPMI ( AGENTS(I), '#', I, AGENTS(I) )

         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Since these watches are new, each agent should be notified
!     of an update. Verify this.
!
      DO I = 1, NAGENT

         QNAME = '(0) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(1) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Examine the watcher system; see whether we have the
!     expected contents.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the agents for VAR(1).
!
      CALL SSIZEC ( MXNOTE, AGTSET )

      CALL ZZGAPOOL ( VARS(1), UWVARS, UWPTRS, UWPOOL, UWAGNT, AGTSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We must sort the expected agent array before the comparison.
!
      CALL MOVEC  ( AGENTS, NAGENT, XAGSET(1) )

      CALL VALIDC ( MXNOTE, NAGENT, XAGSET )

      CALL CHCKAC ( 'AGTSET', AGTSET(1), '=', XAGSET(1), NAGENT, OK )

!
!     The order of deletion can be important, so we're going to
!     delete the watches in:
!
!         - order of insertion
!         - reverse order of insertion
!         - alphabetically sorted order
!
!     We'll re-set the watches in the original order each time we
!     re-set them.
!
!     Delete watches in order of insertion.
!
      DO I = 1, NAGENT

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


!
!     Re-set watches in the original order.
!
      DO I = 1, NAGENT

         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = '(2) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(3) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO


!
!     Delete watches in the reverse of the order of insertion.
!
      DO I = NAGENT, 1, -1

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


!
!     Re-set watches in the original order.
!
      DO I = 1, NAGENT

         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = '(4) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(5) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Delete watches in the reverse of the order of insertion.
!
      DO I = NAGENT, 1, -1

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (2)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


!
!     Re-set watches in the original order.
!
      DO I = 1, NAGENT

         CALL SWPOOL ( AGENTS(I), 1, VARS(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = '(6) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(7) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO


!
!     Delete watches in sorted order.
!
      CALL ORDERC ( AGENTS, NAGENT, ORDVEC )

      DO I = 1, NAGENT

         J = ORDVEC(I)

         CALL DWPOOL ( AGENTS(J) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (3)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Set multiple watches on multiple variables; ' &
      //           'variables do not exist in the kernel pool. ' &
      //           'Each agent watches the same variables.'       )



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

!
!     Since these watches are new, each agent should be notified
!     of an update. Verify this.
!
      DO I = 1, NAGENT

         QNAME = '(0) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(1) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Examine the watcher system; see whether we have the
!     expected contents.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     We must sort the expected agent array before the comparison.
!
      CALL MOVEC  ( AGENTS, NAGENT, XAGSET(1) )

      CALL VALIDC ( MXNOTE, NAGENT, XAGSET )


!
!     Get the agents for each kernel variable; compare these to
!     our sorted agent set.
!
      DO I = 1, NVARS

         CALL ZZGAPOOL ( VARS(I), UWVARS, UWPTRS, &
                         UWPOOL,  UWAGNT, AGTSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAC ( 'AGTSET', AGTSET(1), '=', XAGSET(1), NAGENT, OK )

      END DO

!
!     Delete watches in order of insertion.
!
      DO I = 1, NAGENT

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )

!
!     Re-set the watches.
!
      DO I = 1, NAGENT

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = '(2) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(3) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Delete watches in the reverse of the order of insertion.
!
      DO I = NAGENT, 1, -1

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


!
!     Re-set the watches.
!
      DO I = 1, NAGENT

         CALL SWPOOL ( AGENTS(I), NVARS, VARS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = '(4) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(5) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Delete watches in sorted order.
!
      CALL ORDERC ( AGENTS, NAGENT, ORDVEC )

      DO I = 1, NAGENT

         J = ORDVEC(I)

         CALL DWPOOL ( AGENTS(J) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (3)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Set multiple watches on multiple variables; ' &
      //           'variables do not exist in the kernel pool. ' &
      //           'Each agent watches a different set ' &
      //           'of variables.'                               )


!
!     The sets of watched variables are disjoint in this test case.
!     Each variable is watched by only one agent.
!
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

!
!     Since these watches are new, each agent should be notified
!     of an update. Verify this.
!
      DO I = 1, NAGENT

         QNAME = '(0) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .TRUE., OK )

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(1) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Examine the watcher system; see whether we have the
!     expected contents.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the agent (note: singular) for each kernel variable.
!
      DO I = 1, NAGENT

         DO J = 1, NVARS
!
!           Re-create the name VARS(J).
!
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Get the current watch state.
!
            CALL ZZGAPOOL ( VARS(J), UWVARS, UWPTRS, &
                            UWPOOL,  UWAGNT, AGTSET )
            CALL CHCKXC ( .FALSE., ' ', OK )


            QNAME = 'CARDC(AGTSET) (#,#)'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( QNAME, CARDC(AGTSET), '=', 1, 0, OK )

            CALL CHCKSC ( 'AGTSET (0)', AGTSET(1), '=', AGENTS(I), OK )

         END DO

      END DO

!
!     Delete watches in order of insertion.
!
      DO I = 1, NAGENT

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )



!
!     Re-set the watches.
!
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

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(3) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Delete watches in the reverse of the order of insertion.
!
      DO I = NAGENT, 1, -1

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )


!
!     Re-set the watches.
!
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

!
!        Make sure a second test indicates "no update."
!
         QNAME = '(5) Update #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, UPDATE, .FALSE., OK )

      END DO

!
!     Delete watches in sorted order.
!
      CALL ORDERC ( AGENTS, NAGENT, ORDVEC )

      DO I = 1, NAGENT

         J = ORDVEC(I)

         CALL DWPOOL ( AGENTS(J) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (2)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Set multiple watches on multiple variables; ' &
      //           'variables do not exist in the kernel pool. ' &
      //           'Variables are watched by multiple agents.' )


!
!     The sets of watched variables are disjoint in this test case.
!     Each variable is watched by multiple agents.
!
      NSET   =  5
      NVARS  = 21
      NAGENT = 11

      DO I = 1, NSET

         DO J = 1, NVARS
!
!           Each variable name includes I and J.
!
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO


         DO J = 1, NAGENT
!
!           Each agent name includes I and J.
!
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Set a watch for the Jth agent on the Ith
!           set of variables.
!
            CALL SWPOOL ( AGENTS(J), NVARS, VARS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

!
!     Since these watches are new, each agent should be notified
!     of an update. Verify this.
!
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

!
!           Make sure a second test indicates "no update."
!
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


!
!     Examine the watcher system; see whether we have the
!     expected contents.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the agent (note: singular) for each kernel variable.
!
      DO I = 1, NSET
!
!        Re-create the agent and variable names.
!
         DO J = 1, NVARS
!
!           Each variable name includes I and J.
!
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Create an agent set while we're creating agent names.
!
         CALL SCARDC ( 0, XAGSET )

         DO J = 1, NAGENT
!
!           Each agent name includes I and J.
!
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL INSRTC ( AGENTS(J), XAGSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO



         DO J = 1, NVARS
!
!           Get the current watch state.
!
            CALL ZZGAPOOL ( VARS(J), UWVARS, UWPTRS, &
                            UWPOOL,  UWAGNT, AGTSET )
            CALL CHCKXC ( .FALSE., ' ', OK )


            QNAME = 'CARDC(AGTSET) (#,#)'
            CALL REPMI ( QNAME, '#', I, QNAME )
            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( QNAME, CARDC(AGTSET), '=', NAGENT, 0, OK )

            CALL CHCKAC ( 'AGTSET (0)', AGTSET(1), '=', &
                                        XAGSET(1), NAGENT, OK )

         END DO

      END DO

!
!     Delete watches in order of insertion.
!
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

!
!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (0)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )



!
!     Re-set the watches.
!
      DO I = 1, NSET


         DO J = 1, NVARS
!
!           Each variable name includes I and J.
!
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO


         DO J = 1, NAGENT
!
!           Each agent name includes I and J.
!
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Set a watch for the Jth agent on the Ith
!           set of variables.
!
            CALL SWPOOL ( AGENTS(J), NVARS, VARS )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Check update status.
!
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

!
!           Make sure a second test indicates "no update."
!
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


!
!     Delete watches in the reverse of the order of insertion.
!
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


!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )



!
!     Re-set the watches.
!
!     This time, create a set of all agents.
!
      CALL SCARDC ( 0, AGTSET )

      DO I = 1, NSET

         DO J = 1, NVARS
!
!           Each variable name includes I and J.
!
            VARS(J) = 'VARS#_#'
            CALL REPMI ( VARS(J), '#', I, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( VARS(J), '#', J, VARS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO


         DO J = 1, NAGENT
!
!           Each agent name includes I and J.
!
            AGENTS(J) = 'AGENTS#_#'
            CALL REPMI ( AGENTS(J), '#', I, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( AGENTS(J), '#', J, AGENTS(J) )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Set a watch for the Jth agent on the Ith
!           set of variables.
!
            CALL SWPOOL ( AGENTS(J), NVARS, VARS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Add this agent to the agent set.
!
            CALL INSRTC ( AGENTS(J), AGTSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Check update status.
!
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

!
!           Make sure a second test indicates "no update."
!
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


!
!     Delete watches in sorted order.
!
      DO I = 1, CARDC( AGTSET )

         CALL DWPOOL ( AGTSET(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!     Check the watch system status.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )






!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Try to delete watch with update pending' )


      AGENT     = 'AGENT1'
      NVARS     = 1
      VARS(1)   = 'VAR1'
      DPVALS(1) = -999.D0

      CALL PDPOOL ( VARS(1), 1, DPVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SWPOOL ( AGENT, NVARS, VARS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the watch.
!
      CALL DWPOOL ( AGENT )
      CALL CHCKXC ( .TRUE., 'SPICE(UPDATEPENDING)',   OK )

!
!     Clean up the watch.
!
      CALL CVPOOL ( AGENT, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DWPOOL ( AGENT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Determine the number of watches set outside of ' &
      //           'this test family.'                               )


      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NUSED  = CARDC(UWVARS)
      NAVAIL = MAXVAR - NUSED

!
!     Make sure we have enough watches available so the rest
!     of our tests make sense. The exact number of available
!     slots in the watched variable set depends not only on
!     the Toolkit version but the position of this test family
!     in the sequence of TSPICE tests.
!

      CALL CHCKSI ( 'NAVAIL', NAVAIL, '>', 4000, 0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Kernel variable set overflow: large ' &
      //           'variable set'                         )

      DO I = 1, NAVAIL + 1

         VARS(I) = 'VAR(#)'
         CALL REPMI ( VARS(I), '#', I, VARS(I) )

      END DO

      CALL SWPOOL ( 'AGENT1', NAVAIL+1, VARS )
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARSETOVERFLOW)', OK )

!
!     We expect that no new watch was set. Check the watch
!     system.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Kernel variable set overflow: single ' &
      //           'variable'                             )

      DO I = 1, NAVAIL

         VARS(I) = 'VAR(#)'
         CALL REPMI ( VARS(I), '#', I, VARS(I) )

         CALL SWPOOL ( 'AGENT1', 1, VARS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     The last straw.
!
      CALL SWPOOL ( 'AGENT1', 1, 'last straw' )
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARSETOVERFLOW)', OK )

!
!     Clean up.
!
      CALL CVPOOL ( 'AGENT1', UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DWPOOL ( 'AGENT1' )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the watch system.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Too many agents for one variable' )

      DO I = 1, MAXAGT

         AGENTS(I) = 'AGENTS(#)'
         CALL REPMI ( AGENTS(I), '#', I, AGENTS(I) )

         CALL SWPOOL ( AGENTS(I), 1, 'VAR1' )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     The last straw.
!
      CALL SWPOOL ( 'last straw', 1, 'VAR1' )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOMANYWATCHES)', OK )

!
!     Clean up.
!
      DO I = 1, MAXAGT

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Agent pool overflow' )

!
!     Start out by finding out how much room is in
!     the agent list.
!
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

!
!     The last straw.
!
      CALL SWPOOL ( 'last straw', MAXAGT, VARS )
      CALL CHCKXC ( .TRUE., 'SPICE(AGENTLISTOVERFLOW)', OK )

!
!     Clean up.
!
      DO I = 1, N

         CALL CVPOOL ( AGENTS(I), UPDATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DWPOOL ( AGENTS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check the watch system.
!
      CALL ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAC ( 'UWVARS (1)', UWVARS(1), '=', &
                    OKVSET(1),    ONVARS,    OK   )

      N = MXNOTE - LNKNFN(UWPOOL)

      CALL CHCKSI ( 'N', N, '=', ONAGNT, 0, OK )




      CALL T_SUCCESS ( OK )

      RETURN
      END
