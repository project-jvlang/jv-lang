new Object() {
    java.util.List run() {
        try (var __jvSequence = new JvSequence<>((numbers).stream().map(value -> value).filter(value -> true))) {
            return __jvSequence.toStream().toList();
        }
    }
}.run()
