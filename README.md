Enigma
======

This project implements the Enigma encryption system used by the Germans during
World War 2.

It is a Scala port of a Ruby version contained in `enigma.rb`, which was
originally written by Albert Still for his post [Understanding the Enigma 
machine with 30 lines of Ruby. Star of the 2014 film "The Imitation 
Game"](http://red-badger.com/blog/2015/02/23/understanding-the-enigma-machine-with-30-lines-of-ruby-star-of-the-2014-film-the-imitation-game/).

License
-------

This is of course released into the public domain.

Running
-------

    sbt 'run "this is your message"'

It will output the randomly-generated plugboard, reflector, and rotors.

As of 1 March 2015, I have not yet implemented decryption, or, really, how to
supply the plugboard, reflector, and rotors used for encryption.
