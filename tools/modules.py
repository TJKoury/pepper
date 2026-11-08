"""
Which SPICELIB routines go into which PEPPER module (used by
tools/modularize.py). Modules are listed in dependency order: a module's
routines may use only modules listed before it.

Steps to move the routines of a new module (MODULE = e.g. pepper_cells):

    python tools/modularize.py MODULE
    python tools/add_intents.py src/MODULE.f90
    pixi run python tools/mark_pure.py src/MODULE.f90
    python tools/document_procedures.py --remove-brief-io \
        src/MODULE.f90 src/legacy/pepper_legacy_<name>.f90
    python tools/modernize_syntax.py src
    python tools/wrap_docs.py src
    pixi run tspice

Steps for a context module (CONTEXT_MODULES; intents, documentation and
the Brief_I/O removal are done by modularize_ctx.py itself):

    python tools/data_to_parameter.py src/spicelib/<file>  (read-only tables)
    python tools/modularize_ctx.py --dry-run MODULE     (checks the calls)
    python tools/modularize_ctx.py MODULE
    python tools/modernize_syntax.py src
    python tools/wrap_docs.py src
    pixi run tspice

Constants computed on the first call (a saved FIRST flag) are rewritten as
parameters by hand before the move.
"""

# Constant functions of SPICELIB that are named constants in
# pepper_constants: name -> kind ("wp" real, "ip" integer).
CONSTANTS = {
    **{n: "wp" for n in "b1900 b1950 clight dpmax dpmin dpr halfpi j1900 j1950 "
                        "j2000 j2100 jyear pi rpd spd twopi tyear".split()},
    **{n: "ip" for n in "intmax intmin".split()},
}

MODULES = {
    "pepper_constants": sorted(CONSTANTS),
    "pepper_arrays": """
        alltru bschoc bschoi bsrchc bsrchd bsrchi clearc cleard cleari fillc
        filld filli isordv isrchc isrchd isrchi lstcld lstcli lstlec lstled
        lstlei lstltc lstltd lstlti maxac maxad maxai minac minad minai movec
        moved movei notru orderc orderd orderi prodad prodai reordc reordd
        reordi reordl rmdupc rmdupd rmdupi sameai shellc shelld shelli somfls
        somtru sumad sumai swapc swapd swapi
        touchc touchd touchi touchl
        """.split(),
    "pepper_math": """
        approx axisar azlrec brcktd brckti chbder chbint chbval cyllat cylrec
        cylsph dcbrt det drdcyl drdlat drdsph ducrss dvcrss dvdot dvhat dvnorm
        el2cgv even exact frame gcd ident insang invert latcyl latrec latsph
        mequ mequg mtxm mtxmg mtxv mtxvg mxm mxmg mxmt mxmtg mxv mxvg odd
        opsgnd opsgni pl2nvc pl2nvp pl2psv pltexp pltnrm polyds q2m qdq2av qxq
        radrec rav2xf recazl reccyl reclat recrad recsph sharpr smsgnd smsgni
        sphcyl sphlat sphrec trace traceg ucrss unorm unormg vadd vaddg vcrss
        vdist vdistg vdot vdotg vequ vequg vhat vhatg vhatip vlcom vlcom3
        vlcomg vminug vminus vnorm vnormg vpack vperp vproj vprojg vrel vrelg
        vrotv vscl vsclg vsclip vsep vsepg vsub vsubg vtmv vtmvg vupack vzero
        vzerog xf2rav xpose xposeg xpsgip zzmsxf zzrxr
        """.split(),
    "pepper_strings": """
        astrip bedec beint benum beuns cmprss cpos cposr eqstr esrchc fndnwd
        frstnb frstnp frstpc kxtrct lastnb lastpc lbuild lcase ljucrs ljust
        lparse lparsm ltrim lxqstr matchw nblen nbwid ncpos ncposr nextwd nthwd
        parsqs pcwid pos posr prefix quote replch replwd repmc rjust rtrim
        samch samsub scanrj shiftl shiftr sigdgt suffix ucase wdcnt wdindx
        zzinssub zzrepsub intstr dpstr
        """.split(),
    "pepper_errors": ["expln"],
}

# Routines that need the context (they signal errors): moved by
# tools/modularize_ctx.py into submodules of pepper_instance. In dependency
# order, like MODULES.
CONTEXT_MODULES = {
    "pepper_cells": """
        chbase enchar dechar excess
        cardc cardd cardi sizec sized sizei scardc scardd scardi
        ssizec ssized ssizei validc validd validi
        appndc appndd appndi copyc copyd copyi insrtc insrtd insrti
        removc removd removi elemc elemd elemi ordc ordd ordi setc setd seti
        unionc uniond unioni interc interd interi diffc diffd diffi
        sdiffc sdiffd sdiffi
        zzwninsd wncard wncomd wncond wndifd wnelmd wnexpd wnextd wnfetd
        wnfild wnfltd wnincd wninsd wnintd wnreld wnsumd wnunid wnvald
        """.split(),
    "pepper_numerics": """
        dacosh dacosn dasine datanh dazldr dcyldr dlatdr drdazl drdgeo dsphdr
        eqncpv edpnt ednmpt elltof hrmesp hrmint lgresp lgrind lgrint chbigr
        inelpl inrypl invort invstm isrot kepleq kpsolv m2q nplnpt npsgpt
        nvc2pl nvp2pl psv2pl pltar pltnp pltvol qderiv raxisa rquad rmaind
        rmaini vprjp vprjpi stelab stlabx xposbl
        """.split(),
    "pepper_array_ops": """
        cyacip cyadip cyaiip cyclac cyclad cyclai inslac inslad inslai
        remlac remlad remlai swapac swapad swapai packac packad packai
        lnkan lnkfsl lnkhl lnkila lnkilb lnkini lnknfn lnknxt lnkprv lnksiz
        lnktl lnkxsl
        """.split(),
    "pepper_text": """
        cyclec repmd repmi repsub remsub inssub shiftc chckid countc hx2int
        locati locln
        """.split(),
}
