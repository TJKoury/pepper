!$Procedure ZZEKIXLK ( EK, look up record pointer in EK index )

      SUBROUTINE ZZEKIXLK ( HANDLE, COLDSC, KEY, RECPTR )

! $ Abstract
!
!     Look up a specified record pointer from an EK index.
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

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'

      INTEGER               HANDLE
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               KEY
      INTEGER               RECPTR

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
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               ITYPE
      INTEGER               Q
      INTEGER               R
      INTEGER               TREE

!
!     Use discovery check-in.
!
      ITYPE  =  COLDSC ( IXTIDX )


      IF ( ITYPE .EQ. 1 ) THEN
!
!        For type 1 indexes, the index pointer is the root node of
!        a B*-tree.  Just use the tree look up routine.
!
         TREE  = COLDSC ( IXPIDX )

         CALL ZZEKTRDP ( HANDLE, TREE, KEY, RECPTR )


      ELSE IF ( ITYPE .EQ. 2 ) THEN
!
!        For type 2 indexes, the index pointer is the base address
!        of the index.  We must compute the offset from this base to
!        the index element having ordinal position KEY.
!
         BASE    =    COLDSC(IXPIDX)
         Q       =    (KEY-1)  / IPSIZE
         R       =    KEY  - Q * IPSIZE
         ADDRSS  =    BASE + Q * PGSIZI + R

         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, RECPTR )

      ELSE
!
!        Sorry, no other types of indexes are supported.
!
         CALL CHKIN  ( 'ZZEKIXLK'                           )
         CALL SETMSG ( 'The index type # is not supported.' )
         CALL ERRINT ( '#',   ITYPE                         )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                 )
         CALL CHKOUT ( 'ZZEKIXLK'                           )
         RETURN

      END IF


      END
