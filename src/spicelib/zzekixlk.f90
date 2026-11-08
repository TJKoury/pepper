!$Procedure ZZEKIXLK ( EK, look up record pointer in EK index )

      subroutine zzekixlk ( handle, coldsc, key, recptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Look up a specified record pointer from an EK index.
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

      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekpage.inc'

      integer(ip)           handle
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           key
      integer(ip)           recptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     COLDSC     I   Column descriptor.
!     KEY        I   Key.
!     RECPTR     O   Record pointer.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     COLDSC         is the column descriptor of the column to which
!                    the index of interest belongs.
!
!     KEY            is the key of the record pointer of interest.  This
!                    key is the ordinal position of the record pointer
!                    in the index.
!
! $ Detailed_Output
!
!     RECPTR         is the record pointer corresponding to the input
!                    key.  This pointer gives the base address of
!                    the record pointer structure for the record having
!                    ordinal position KEY within the specified column,
!                    where the order is defined by the column's index.
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
!     2)  If KEY is out of range, the error will be diagnosed by
!         routines called by this routine.
!
!     3)  If an I/O error occurs while reading or the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine finds the record pointer for a record having a
!     specified ordinal position in a column, where the order is
!     defined by the column's index.
!
! $ Examples
!
!     See EKSRCH.
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
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


!
!     Local variables
!
      integer(ip)           addrss
      integer(ip)           base
      integer(ip)           itype
      integer(ip)           q
      integer(ip)           r
      integer(ip)           tree

!
!     Use discovery check-in.
!
      itype  =  coldsc ( ixtidx )


      if ( itype == 1 ) then
!
!        For type 1 indexes, the index pointer is the root node of
!        a B*-tree.  Just use the tree look up routine.
!
         tree  = coldsc ( ixpidx )

         call zzektrdp ( handle, tree, key, recptr )


      else if ( itype == 2 ) then
!
!        For type 2 indexes, the index pointer is the base address
!        of the index.  We must compute the offset from this base to
!        the index element having ordinal position KEY.
!
         base    =    coldsc(ixpidx)
         q       =    (key-1)  / ipsize
         r       =    key  - q * ipsize
         addrss  =    base + q * pgsizi + r

         call dasrdi ( handle, addrss, addrss, recptr )

      else
!
!        Sorry, no other types of indexes are supported.
!
         call chkin  ( 'ZZEKIXLK'                           )
         call setmsg ( 'The index type # is not supported.' )
         call errint ( '#',   itype                         )
         call sigerr ( 'SPICE(INVALIDTYPE)'                 )
         call chkout ( 'ZZEKIXLK'                           )
         return

      end if


      end
