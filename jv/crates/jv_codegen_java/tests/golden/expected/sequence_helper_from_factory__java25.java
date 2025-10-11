new Object() {
    java.util.List run() {
        try (var __jvSequence = new JvSequence<>(SequenceFactory.fromIterable(numbers).map((value) -> value))) {
            return __jvSequence.toStream().toList();
        }
    }
}.run()
