# Wikilate

Use different language versions of Wikipedia for translation from the command line

## Usage

    wikilate PHRASE [-s SOURCE_LANG] [-d DEST_LANG[,DEST_LANG...]]

Translates given <code>PHRASE</code> from <code>SOURCE_LANG</code> (<code>en</code> by default)
to one or more <code>DEST_LANG</code>s, (<code>de</code>, <code>fr</code>, <code>es</code>
and <code>pl</code> by default), using titles of Wikipedia articles.

Example:

    $ wikilate lentil -d de,pl,es
    de: Linse (Botanik)
    es: Lens culinaris
    pl: Soczewica jadalna

## Installation

You can use standard Cabal:

    $ git clone git://github.com/Xion/wikilate.git
    $ cd wikilate
    $ cabal install

If your <code>~/.cabal/bin</code> directory is in <code>$PATH</code>, you'll should be able
to use the <code>wikilate</code> command now.

Alternatively, you can run the source file directly via <code>runghc</code>. In that case,
it's convenient to define a shell alias (e.g. in _.bash\_aliases_):

    alias wikilate='runghc ~/wikilate/Wikilate.hs'

The <code>~/wikilate/</code> part should be appropriately adjusted, of course.

----

This program is written and maintained by Karol Kuczmarski, <karol.kuczmarski@gmail.com>.