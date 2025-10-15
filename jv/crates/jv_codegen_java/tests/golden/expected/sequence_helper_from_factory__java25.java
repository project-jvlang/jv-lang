new Object() {
    java.util.List run() {
        try (var __jvSequence = new JvSequence<>(Sequence.sequenceFromIterable(numbers).map((value) -> value))) {
            return __jvSequence.toStream().toList();
        }
    }
}.run()
