!$Procedure   ZZEKPGCH ( EK, paging system access check )

      SUBROUTINE ZZEKPGCH ( HANDLE, ACCESS )

! $ Abstract
!
!     Check that an EK is valid for a specified type of access by the
!     paging system.
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
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekpage.inc'
      INCLUDE 'ekarch.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         ACCESS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     ACCESS     I   Access type.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The specified file is to be
!                    checked to see whether it is a valid paged EK and
!                    whether it is open for the specified type of
!                    access.
!
!     ACCESS         is a short string indicating the type of access
!                    desired.  Possible values are 'READ' and 'WRITE'.
!
!                    Leading and trailing blanks in ACCESS are ignored,
!                    and case is not significant.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If the EK architecture version is not current, the error
!         SPICE(WRONGARCHITECTURE) is signaled.
!
!     3)  If the DAS logical address ranges occupied by the EK are
!         not consistent with those recorded by the paging system,
!         the error SPICE(INVALIDFORMAT) is signaled.
!
!     4)  If the EK is not open for the specified type of access, the
!         error will be diagnosed by routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine centralizes a validation check performed by many
!     EK routines.  The EK designated by HANDLE is tested to see
!     whether some aspects of its structure are valid, and whether
!     the specified type of access (read or write) is allowed.
!     The tests performed are:
!
!        - Is the file a DAS file open for the specified type of access?
!
!        - Is the file's EK architecture version correct?
!
!        - Are the DAS address ranges in use consistent with those
!          recorded in the file by the paging system?
!
!     If the file fails any test, an error is signaled.
!
! $ Examples
!
!     See EKINSR.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               ID
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               NPC
      INTEGER               NPD
      INTEGER               NPI
      INTEGER               TOPC
      INTEGER               TOPD
      INTEGER               TOPI



      CALL CHKIN ( 'ZZEKPGCH' )

!
!     Check whether the DAS is opened for the specified access method.
!
      CALL DASSIH ( HANDLE, ACCESS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGCH' )
         RETURN
      END IF

!
!     Make sure the DAS file is of the right type.
!
      CALL DASRDI ( HANDLE, EPARCH, EPARCH, ID )

      IF ( ID .NE. ARCHID ) THEN

         CALL SETMSG ( 'File # has architecture #, which is invalid '// &
                       'for paged access.  You are using EK software '// &
                       'version #.'                                  )
         CALL ERRHAN ( '#',   HANDLE                                 )
         CALL ERRINT ( '#',   ID                                     )
         CALL ERRINT ( '#',   ARCHID                                 )
         CALL SIGERR ( 'SPICE(WRONGARCHITECTURE)'                    )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN

      END IF

!
!     Obtain the page counts.  Set the `top' addresses.
!
      CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )
      CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )
      CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )

      TOPC  =  NPC * PGSIZC
      TOPD  =  NPD * PGSIZD
      TOPI  =  NPI * PGSIZI + PGBASI

!
!     Verify that the last addresses in use are consistent with the
!     `top' addresses known to this system.
!
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )

      IF ( LASTC .GT. TOPC ) THEN

         CALL SETMSG ( 'File # has last char address #; `top'' = #.' )
         CALL ERRHAN ( '#',   HANDLE                                 )
         CALL ERRINT ( '#',   LASTC                                  )
         CALL ERRINT ( '#',   TOPC                                   )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN

      ELSE IF ( LASTD .GT. TOPD ) THEN

         CALL SETMSG ( 'File # has last d.p. address #; `top'' = #.' )
         CALL ERRHAN ( '#',   HANDLE                                 )
         CALL ERRINT ( '#',   LASTD                                  )
         CALL ERRINT ( '#',   TOPD                                   )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN

      ELSE IF ( LASTI .GT. TOPI ) THEN

         CALL SETMSG ( 'File # has last int. address #; `top'' = #.' )
         CALL ERRHAN ( '#',   HANDLE                                 )
         CALL ERRINT ( '#',   LASTI                                  )
         CALL ERRINT ( '#',   TOPI                                   )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZEKPGCH' )
      RETURN
      END
