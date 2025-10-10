(numbers).stream().reduce((acc, value) -> acc + value).orElseThrow(() -> new IllegalArgumentException("Sequence reduce() on empty source"))
