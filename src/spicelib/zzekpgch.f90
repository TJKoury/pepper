!$Procedure   ZZEKPGCH ( EK, paging system access check )

      subroutine zzekpgch ( handle, access )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Check that an EK is valid for a specified type of access by the
!     paging system.
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

      include 'ekpage.inc'
      include 'ekarch.inc'
      include 'ektype.inc'

      integer(ip)           handle
      character(*)          access

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
      logical               failed

!
!     Local variables
!
      integer(ip)           id
      integer(ip)           lastc
      integer(ip)           lastd
      integer(ip)           lasti
      integer(ip)           npc
      integer(ip)           npd
      integer(ip)           npi
      integer(ip)           topc
      integer(ip)           topd
      integer(ip)           topi

!
!     Check whether the DAS is opened for the specified access method.
!
      call dassih ( handle, access )

      if ( failed() ) return

!
!     Make sure the DAS file is of the right type.
!
      call dasrdi ( handle, eparch, eparch, id )

      if ( id /= archid ) then

         call setmsg ( 'File # has architecture #, which is invalid '// &
                       'for paged access.  You are using EK software '// &
                       'version #.'                                  )
         call errhan ( '#',   handle                                 )
         call errint ( '#',   id                                     )
         call errint ( '#',   archid                                 )
         call sigerr ( 'SPICE(WRONGARCHITECTURE)'                    )
         return

      end if

!
!     Obtain the page counts.  Set the `top' addresses.
!
      call dasrdi ( handle, epnpc, epnpc, npc )
      call dasrdi ( handle, epnpd, epnpd, npd )
      call dasrdi ( handle, epnpi, epnpi, npi )

      topc  =  npc * pgsizc
      topd  =  npd * pgsizd
      topi  =  npi * pgsizi + pgbasi

!
!     Verify that the last addresses in use are consistent with the
!     `top' addresses known to this system.
!
      call daslla ( handle, lastc, lastd, lasti )

      if ( lastc > topc ) then

         call setmsg ( 'File # has last char address #; `top'' = #.' )
         call errhan ( '#',   handle                                 )
         call errint ( '#',   lastc                                  )
         call errint ( '#',   topc                                   )
         call sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         return

      else if ( lastd > topd ) then

         call setmsg ( 'File # has last d.p. address #; `top'' = #.' )
         call errhan ( '#',   handle                                 )
         call errint ( '#',   lastd                                  )
         call errint ( '#',   topd                                   )
         call sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         return

      else if ( lasti > topi ) then

         call setmsg ( 'File # has last int. address #; `top'' = #.' )
         call errhan ( '#',   handle                                 )
         call errint ( '#',   lasti                                  )
         call errint ( '#',   topi                                   )
         call sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         return

      end if

      return
      end
