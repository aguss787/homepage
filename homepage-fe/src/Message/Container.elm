module Message.Container exposing (..)

import Bootstrap.Navbar as Navbar

type Message
    = NavbarMsg Navbar.State
