{
  let [value, setValue] = useState(50);
  return (
    <>
      <p>
        <input
          type="range"
          min={0}
          max={100}
          value={value}
          onChange={(event) => setValue(event.target.valueAsNumber)}
        />
      </p>

      <p>The current value is: {value}.</p>
    </>
  );
}
