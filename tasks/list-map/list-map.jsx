(() => {
  let items = ["Milk", "Eggs", "Cheese"];
  return (
    <>
      <p>Today I am going shopping for:</p>
      <ul>
        {items.map((item, i) => (
          <li key={i}><p>{item}</p></li>
        ))}
      </ul>
    </>
  );
})();
