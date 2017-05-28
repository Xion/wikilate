# Wikilate

Translate phrases from command line using Wikipedia

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

You can use standard Stack:

    $ git clone git://github.com/Xion/wikilate.git
    $ cd wikilate
    $ stack install

If your <code>~/.local/bin</code> directory is in <code>$PATH</code>, you should be able
to use the <code>wikilate</code> command now.

