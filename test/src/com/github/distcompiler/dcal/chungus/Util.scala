package test.com.github.distcompiler.dcal.chungus

// a tricky way to get an iterator that runs code when consumed but never yields anything
private[chungus] def iterSentinel(fn: =>Unit): Iterator[Nothing] =
  Iterator.unfold(None) { _ =>
    fn
    None
  }
